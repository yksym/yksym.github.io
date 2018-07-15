module Main where

import Prelude

import Data.Array (catMaybes)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (Tuple2, Tuple3, tuple2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Int (round)
--import Data.Function ((#))


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
import Data.Lens (Lens', lens, _1, _2, (^.), use, (.=), (.~))
import Data.Profunctor.Strong(class Strong)

type PosVel = Tuple2 Number Number
type PosVelAcc = Tuple3 Number Number Number

d0 :: forall p a b c. Strong p => p a b -> p (Tuple a c) (Tuple b c)
d0 = _1

d1 :: forall p a b c d. Strong p => p b d -> p (Tuple a (Tuple b c)) (Tuple a (Tuple d c))
d1 = _2 <<< _1

d2 :: forall p a b c d e. Strong p => p b a -> p (Tuple c (Tuple e (Tuple b d))) (Tuple c (Tuple e (Tuple a d)))
d2 = _2 <<< _2 <<< _1

dt :: Number
dt = 0.01

rdt :: Number
rdt = 1.0 / dt

type  State =
    { kp              :: Number
    , ki              :: Number
    , kd              :: Number
    , a               :: Number
    , noise           :: Number
    , input           :: PosVelAcc
    , target          :: PosVel
    }

_input :: Lens' State PosVelAcc
_input = lens _.input $ _ {input = _}

initialState :: State
initialState =
             { kp : zero
             , ki : zero
             , kd : zero
             , a  : zero
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

render :: State -> H.ComponentHTML Query
render state = 
  HH.div_
  [ renderSlider "Kp"     {min : -100.0, max : 100.0, step : 1.0, value : state.kp,               handler : Just $ ParamChange $ _ {kp = _} }
  , renderSlider "Ki"     {min : -100.0, max : 100.0, step : 1.0, value : state.ki,               handler : Just $ ParamChange $ _ {ki = _} }
  , renderSlider "Kd"     {min : -100.0, max : 100.0, step : 1.0, value : state.kd,               handler : Just $ ParamChange $ _ {kd = _} }
  , renderSlider "a"      {min : -100.0, max : 100.0, step : 1.0, value : state.a,                handler : Just $ ParamChange $ _ {a  = _} }
  , renderSlider "noise"  {min : -100.0, max : 100.0, step : 1.0, value : state.noise,            handler : Just $ ParamChange $ _ {noise = _} }
  , renderSlider "input"  {min : -100.0, max : 100.0, step : 1.0, value : state^.(_input <<< _1), handler : Just $ InputChange}
  , renderSlider "target" {min : -100.0, max : 100.0, step : 1.0, value : state.target^._1,       handler : Nothing}
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
                void $ H.modify $ \st -> st { input = updateWithPos (st.input) x }
                pure next
        Tick next -> do
          void $ H.modify $ \st -> st { target = updateWithEuler st }
          pure next

updateWithPos :: PosVelAcc -> Number -> PosVelAcc
updateWithPos prev x = prev # d0 .~ x # d1 .~ x' # d2 .~ x''
    where
    x'  = (x  - (prev ^. d0)) * rdt
    x'' = (x' - (prev ^. d1)) * rdt

--                                                    2
--    Ki X - Ki Y + (((- a) - Kp) Y + Kp X) s + Kd X s
--    -------------------------------------------------
--                         Kd - 1
updateWithEuler :: State -> PosVel
updateWithEuler st = tuple2 y y'
    where
    x0  = st.input ^. d0
    x0' = st.input ^. d1
    x0''= st.input ^. d2
    y0  = st.target ^. d0
    y0' = st.target ^. d1
    denom = st.kd - 1.0
    num   = st.ki * (x0 - y0) - st.a * y + st.kp * (x0' - y0') + st.kd * x0''
    y   = y0 + dt * y0'
    y'  = y0' + dt * (num / denom)



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
  void $ H.liftEffect $ T.setInterval (round $ 1000.0 * dt) $ do
    runHalogenAff $ io.query $ H.action $ Tick

