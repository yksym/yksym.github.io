module Main where

import Prelude

import Data.Array (catMaybes)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)

import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Effect.Timer as T

import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Data.Lens (Lens', lens, _1, _2, (^.), use, (.=))

type F2 = Tuple Number (Tuple Number ( Tuple Number Unit))

d0 :: Lens' F2 Number
d0 = _2 <<< _1

d1 :: Lens' F2 Number
d1 = _2 <<< _1

d2 :: Lens' F2 Number
d2 = _2 <<< _2 <<< _1

type  State =
    { kp              :: Number
    , ki              :: Number
    , kd              :: Number
    , a               :: Number
    , coeff           :: Number
    , noise           :: Number
    , input           :: F2
    , target          :: F2
    }

_input :: Lens' State F2
_input = lens _.input $ _ {input = _}

initialState :: State
initialState =
             { kp : zero
             , ki : zero
             , kd : zero
             , a  : zero
             , coeff : zero
             , noise : zero
             , input : zero
             , target : zero
             }

data Query a = Tick a
    | ParamChange  (State -> Number -> State) String a
    | InputChange  String a

type SliderProp f =
    { min       :: Number
    , max       :: Number
    , step      :: Number
    , value     :: Number
    , handler   :: forall a. Maybe (String -> a -> f a)
    }

renderSlider :: forall p f. String -> SliderProp f -> HH.HTML p (f Unit)
renderSlider name prop =
      HH.div_
      [
          HH.label_
            [ HH.div_ [ HH.text $ name <> " : " <> show prop.value]
            , HH.input $ 
                [ HP.type_ I.InputRange
                , HP.min prop.min
                , HP.max prop.max
                , HP.value $ show prop.value
                , HP.step $ Step prop.step
                ] <> catMaybes [HE.onValueInput <$> HE.input <$> prop.handler ]
            ]
      ]


-- lens?
render :: State -> H.ComponentHTML Query
render state = 
  HH.div_
  [ renderSlider "Kp" {min : -100.0, max : 100.0, step : 1.0, value : state.kp, handler : Just $ ParamChange $ _ {kp = _} }
  , renderSlider "Ki" {min : -100.0, max : 100.0, step : 1.0, value : state.ki, handler : Just $ ParamChange $ _ {ki = _} }
  , renderSlider "Kd" {min : -100.0, max : 100.0, step : 1.0, value : state.kd, handler : Just $ ParamChange $ _ {kd = _} }
  , renderSlider "a"  {min : -100.0, max : 100.0, step : 1.0, value : state.a,  handler : Just $ ParamChange $ _ {a  = _} }
  , renderSlider "coeff"  {min : -100.0, max : 100.0, step : 1.0, value : state.coeff,  handler : Just $ ParamChange $ _ {coeff = _} }
  , renderSlider "noise"  {min : -100.0, max : 100.0, step : 1.0, value : state.noise,  handler : Just $ ParamChange $ _ {noise = _} }
  , renderSlider "input"  {min : -100.0, max : 100.0, step : 1.0, value : state^.(_input <<< d0),  handler : Just $ InputChange}
  , renderSlider "target" {min : -100.0, max : 100.0, step : 1.0, value : state.target^.d0, handler : Nothing}
  ]

eval :: forall m. (MonadEffect m) => Query ~> H.ComponentDSL State Query Void m
eval = case _ of
        ParamChange update s next -> fromMaybe (pure next) $ do
            x <- fromString s
            pure $ do
                H.liftEffect $ logShow $ show x
                void $ H.modify $ (\a b -> update b a) x
                pure next
        InputChange s next ->  fromMaybe (pure next) $ do
            x <- fromString s
            pure $ do
                input0 <- use $ _input
                _input <<< d0 .= x
                _input <<< d1 .= (x - (input0 ^. d0))
                _input <<< d2 .= (x - (input0 ^. d0)) - (input0 ^. d1)
                pure next
        Tick next -> do
          void $ H.modify $ \st -> st {
              target = rungeKutta st
          }
          pure next

rungeKutta :: State -> F2
rungeKutta _ = zero

component :: forall m. (MonadEffect m) => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }


main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  io <- runUI component unit body
  void $ H.liftEffect $ T.setInterval 1000 $ do
    runHalogenAff $ io.query $ H.action $ Tick

