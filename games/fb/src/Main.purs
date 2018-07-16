module Main where

import Prelude

import Data.Array (catMaybes)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (Tuple3, tuple3)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Int (round)
--import Data.Function(applyN)

--import Debug.Trace (spy, class DebugWarning)

import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)
--import Effect.Console (logShow)
import Effect.Timer as T

import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Data.Lens (Lens', lens, _1, _2, (^.), use, (.=), (.~), (%=))
import Data.Profunctor.Strong(class Strong)

--spy' :: forall a. Show a => a -> a
--spy' x = spy (show x) x

type PosVelAcc = Tuple3 Number Number Number
type Pos3 = Tuple3 Number Number Number

pos :: forall p a b c. Strong p => p a b -> p (Tuple a c) (Tuple b c)
pos = _1

vel :: forall p a b c d. Strong p => p b d -> p (Tuple a (Tuple b c)) (Tuple a (Tuple d c))
vel = _2 <<< _1

acc :: forall p a b c d e. Strong p => p b a -> p (Tuple c (Tuple e (Tuple b d))) (Tuple c (Tuple e (Tuple a d)))
acc = _2 <<< _2 <<< _1

last1 :: Lens' Pos3 Number
last1 = _1

last2 :: Lens' Pos3 Number
last2 = _2 <<< _1

last3 :: Lens' Pos3 Number
last3 = _2 <<< _2 <<< _1

dupLast :: Pos3 -> Pos3
dupLast p = tuple3 (p^.last1) (p^.last1) (p^.last2)

conv :: Number -> Pos3 -> PosVelAcc
conv rdt p = tuple3 x x' x''
    where
    x   = p ^. last1
    x'   = (p ^. last1 - p ^. last2) * rdt
    x''   = (x' - (p ^. last2 - p ^. last3) * rdt) * rdt

dt :: Number
dt = 0.01

-- G(s) = K / ((s - a) (s - b))
type  State =
    { kp              :: Number
    , ki              :: Number
    , kd              :: Number
    , k               :: Number
    , a               :: Number
    , b               :: Number
    , noise           :: Number
    , input           :: Pos3
    , target          :: PosVelAcc
    }

_input :: Lens' State Pos3
_input = lens _.input $ _ {input = _}

_target :: Lens' State PosVelAcc
_target = lens _.target $ _ {target = _}

-- (8.6)
initialState :: State
initialState =
             { kp : 1.0
             , ki : 1.0
             , kd : zero
             , k  : 10.0
             , a  : -1.0
             , b  : -10.0
             , noise : zero
             , input : zero
             , target : zero
             }

stepState :: State
stepState = initialState # _input .~ tuple3 1.0 1.0 1.0

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
  , renderSlider "K"      {min : -100.0, max : 100.0, step : 1.0, value : state.k,                handler : Just $ ParamChange $ _ {k  = _} }
  , renderSlider "a"      {min : -100.0, max : 100.0, step : 1.0, value : state.a,                handler : Just $ ParamChange $ _ {a  = _} }
  , renderSlider "b"      {min : -100.0, max : 100.0, step : 1.0, value : state.b,                handler : Just $ ParamChange $ _ {b  = _} }
  , renderSlider "noise"  {min : -100.0, max : 100.0, step : 1.0, value : state.noise,            handler : Just $ ParamChange $ _ {noise = _} }
  , renderSlider "input"  {min : -10.0,  max : 10.0,  step : 0.1, value : state^.(_input <<< _1), handler : Just $ InputChange}
  , renderSlider "target" {min : -10.0,  max : 10.0 , step : 0.1, value : state.target^._1,       handler : Nothing}
  ]

eval :: forall m. (MonadEffect m) => Query ~> H.ComponentDSL State Query Void m
eval = case _ of
        ParamChange update s next -> fromMaybe (pure next) $ do
            x <- fromString s
            pure $ do
                void $ H.modify $ (flip update) x
                pure next
        InputChange s next ->  fromMaybe (pure next) $ do
            x <- fromString s
            pure $ do
                _input <<< last1 .= x
                pure next
        Tick next -> do
          --st <- use identity
          identity %= updateWithEuler
          _input %= dupLast
          pure next

-- applyN updateWithEuler 100 initialState
updateWithEuler :: State -> State
updateWithEuler st = st # _target .~ tuple3 y y' y''
    where
    input = conv (1.0 / dt) st.input
    x0    = input ^. pos
    x0'   = input ^. vel
    x0''  = input ^. acc
    y0    = st.target ^. pos
    y0'   = st.target ^. vel
    --y0''  = st.target ^. acc
    y0''  = (st.kd*st.k*x0''+ st.a * y0' + st.kp*st.k*(x0'-y0') + st.ki*st.k*(x0 - y0))/(1.0+st.kd*st.k)
    --y0''' = (st.b + st.a) * y0'' - st.kd * st.k * (y0'' + x0'') - st.a*st.b*y' - st.kp*st.k*(y0'+x0') - st.ki * st.k * (y0+x0)
    --y0''' = (st.b + st.a) * y0'' -  st.a*st.b*y' - st.kp*st.k*(y0'+x0') - st.ki * st.k * (y0+x0)
    y     = y0 + dt * y0'
    y'    = y0' + dt * y0''
    y''   = y0'' -- y0'' + dt * y0'''


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

