module Main exposing (main)
import Html exposing (..)
--import Html.Attributes exposing (..)
import Mouse exposing (clicks, Position)
import Time exposing (Time, every, millisecond)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import DrawUtil exposing (..)

type Msg
  = Tick Time
  | AddCookie Index Int
  | DeleteCookie Index Int
  | MoveLCookie Int
  | MoveRCookie Int
  -- | DeleteAllCookie


type alias CookiePosition = { bag: Index, cookie: Index }
type alias MovingCookie = { from: CookiePosition, to: CookiePosition, progress: Float}
type alias Index = Int

type alias Bag = Int
type alias Model = { bag0  : Bag , bag1 : Bag,  moving : List MovingCookie }

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
  let
    model =
      { bag0 = 0
      , bag1 = 0
      , moving = []
      }
  in
    (model, Cmd.none)

screen : String
screen = "0 0 600 600"

cookieSize : Float
cookieSize = 4

cookieGap : Float
cookieGap  = 4

cookiesPerRow : Int
cookiesPerRow = 10

cookiesPerCol : Int
cookiesPerCol = 10

cookiesPerBag : Int
cookiesPerBag = cookiesPerRow * cookiesPerCol

bagGap : Float
bagGap = 50

bagOffset : PositionF
bagOffset = {x=10, y=40}

bagSize : RectSizeF
bagSize = { w = cookieGap + (cookieSize*2 + cookieGap) * toFloat cookiesPerRow
          , h = cookieGap + (cookieSize*2 + cookieGap) * toFloat cookiesPerCol
          }

cookiePos : Index -> Index -> PositionF
cookiePos bIdx cIdx =
    let
        bpos = bagPos bIdx
        xoffset = bpos.x + cookieGap + cookieSize
        yoffset = bpos.y + cookieGap + cookieSize
        scale  = 2*cookieSize + cookieGap
    in
        { x = xoffset + toFloat (cIdx % cookiesPerRow) * scale
        , y = yoffset + toFloat ((cookiesPerBag - 1 - cIdx) // cookiesPerRow) * scale
        }

bagPos : Index -> PositionF
bagPos n = {x=bagOffset.x + toFloat n * (bagSize.w + bagGap), y=bagOffset.y}

view : Model -> Html Msg
view model =
  div [] [ drawModel model ]


drawModel : Model -> Html Msg
drawModel model = svg [ viewBox screen]
    (drawText { x = (bagPos 1).x - bagGap/2 + 5, y = (bagPos 0).y + (bagSize.h / 2) - 10 } 10 "→"  [fill "#000000", onClick (MoveRCookie 1)]
    :: drawText { x = (bagPos 1).x - bagGap + 5, y = (bagPos 0).y + (bagSize.h / 2) - 10 } 10 "←" [fill "#000000", onClick (MoveLCookie 1)]
    :: (drawBag 0 model.bag0
    ++ drawBag 1 model.bag1)
    )


    -- :: drawText { x = bpos.x + 100, y = bpos.y - 10 } 10 "←" [fill "#000000", onClick (MoveCookie 1 0 1)]

drawHoles : Index -> Int -> List (Svg Msg)
drawHoles bIdx n = List.map (\m -> drawCircle (cookiePos bIdx m) cookieSize (fill "none" :: strokeAttr 1 "#000000")) <| List.range 0 (n-1)

drawCookies : Index -> Int -> List (Svg Msg)
drawCookies bIdx n = List.map (\m -> drawCircle (cookiePos bIdx m) cookieSize [fill "#990099"]) <| List.range 0 (n-1)

-- ホールは全部書いておく 100個分
drawBag : Index -> Bag -> List (Svg Msg)
drawBag bIdx n =
    let
        bpos = bagPos bIdx
        bmt  = {bpos | x=bpos.x + bagSize.w / 2}
    in
        [ drawText { bpos | y = bpos.y - 10 } 10 (toString n) [fill "#000000"] -- TODO 移動中の数え方
        , drawText { x = bpos.x + 70, y = bpos.y - 10 } 10 "↑" [fill "#000000", onClick (AddCookie bIdx 1)]
        , drawText { x = bpos.x + 100, y = bpos.y - 10 } 10 "↓" [fill "#000000", onClick (AddCookie bIdx -1)]
        , drawRect bpos bagSize [fill "#c9c44f" ]
        , drawLine bmt {bmt | y = bmt.y + bagSize.h} (strokeAttr 1 "#888888")
        ]
        ++ drawHoles bIdx cookiesPerBag
        ++ drawCookies bIdx n


validate : Model -> Model -> Model
validate default target =
    if target.bag0 < 0 then default
    else if target.bag0 > cookiesPerBag then default
    else if target.bag1 < 0 then default
    else if target.bag1 > cookiesPerBag then default
    else target

-- TODO moveはTickで再帰的に処理
-- TODO move中のadd/delete/move
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        -- Tick t -> 
        AddCookie 0 n -> (validate model {model | bag0 = model.bag0 + n}, Cmd.none)
        AddCookie 1 n -> (validate model {model | bag1 = model.bag1 + n}, Cmd.none)
        MoveRCookie n -> (validate model {model | bag0 = model.bag0 - n, bag1 = model.bag1 + n}, Cmd.none)
        MoveLCookie n -> (validate model {model | bag0 = model.bag0 + n, bag1 = model.bag1 - n}, Cmd.none)
        _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
        Sub.batch
        [ every (100 * millisecond) Tick ]

