module Main exposing (main)
import Html exposing (..)
--import Html.Attributes exposing (..)
import Mouse exposing (clicks, Position)
import Time exposing (Time, every, millisecond, inMilliseconds)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import DrawUtil exposing (..)

type BagIndex = BagL | BagR

type Msg
  = Tick Time
  | AddCookie BagIndex Int
  | DeleteCookie BagIndex Int
  | MoveRLCookie
  | MoveLRCookie

type alias MovingCookie = { from: Index, to: Index, progress: Float, issuedAt: Maybe Time}
type alias Index = Int

type alias Bag = Int
type alias Model = { bagL  : Bag , bagR : Bag,  movingLR : List MovingCookie, movingRL :  List MovingCookie}

bagIndex : BagIndex -> Int
bagIndex bIdx =
    case bIdx of
        BagL -> 0
        BagR -> 1 

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
      { bagL = 0
      , bagR = 0
      , movingLR = []
      , movingRL = []
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

movingTimeSec : Time
movingTimeSec = 800 * millisecond

bagGap : Float
bagGap = 50

bagOffset : PositionF
bagOffset = {x=10, y=40}

bagSize : RectSizeF
bagSize = { w = cookieGap + (cookieSize*2 + cookieGap) * toFloat cookiesPerRow
          , h = cookieGap + (cookieSize*2 + cookieGap) * toFloat cookiesPerCol
          }

cookiePos : BagIndex -> Index -> PositionF
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

countCookies : BagIndex -> Model -> Int
countCookies bIdx model =
    case bIdx of
        BagL -> model.bagL + List.length model.movingRL
        BagR -> model.bagR + List.length model.movingLR

bagPos : BagIndex -> PositionF
bagPos bIdx = {x=bagOffset.x + toFloat (bagIndex bIdx) * (bagSize.w + bagGap), y=bagOffset.y}

viaPos : PositionF
viaPos = { x = (bagPos BagR).x - bagGap/2, y = (bagPos BagL).y - 10}

view : Model -> Html Msg
view model =
  div [] [ drawModel model ]


drawMovingCookie : BagIndex -> BagIndex -> MovingCookie -> Svg Msg
drawMovingCookie srcIdx dstIdx mc = 
    let
        srcPos = cookiePos srcIdx mc.from
        dstPos = cookiePos dstIdx mc.to
        pos    = interp srcPos viaPos dstPos mc.progress
    in
        drawCookie pos

drawModel : Model -> Html Msg
drawModel model = svg [ viewBox screen]
    (drawText { x = (bagPos BagR).x - bagGap/2 + 5, y = (bagPos BagL).y + (bagSize.h / 2) - 10 } 10 "→"  [fill "#000000", onClick MoveLRCookie]
    :: drawText { x = (bagPos BagR).x - bagGap + 5, y = (bagPos BagL).y + (bagSize.h / 2) - 10 } 10 "←" [fill "#000000", onClick MoveRLCookie]
    :: (drawBag BagL model.bagL
    ++ drawBag BagR model.bagR)
    ++ List.map (drawMovingCookie BagL BagR) model.movingLR
    ++ List.map (drawMovingCookie BagR BagL) model.movingRL
    )

drawHoles : BagIndex -> Int -> List (Svg Msg)
drawHoles bIdx n = List.map (\m -> drawCircle (cookiePos bIdx m) cookieSize (fill "none" :: strokeAttr 1 "#000000")) <| List.range 0 (n-1)

drawCookie : PositionF -> Svg Msg
drawCookie pos = drawCircle pos cookieSize [fill "#990099"]

drawCookies : BagIndex -> Int -> List (Svg Msg)
drawCookies bIdx n = List.map (drawCookie << cookiePos bIdx) <| List.range 0 (n-1)

drawBag : BagIndex -> Bag -> List (Svg Msg)
drawBag bIdx n =
    let
        bpos = bagPos bIdx
        bmt  = {bpos | x=bpos.x + bagSize.w / 2}
    in
        [ drawText { bpos | x=bmt.x - 10, y = bpos.y + bagSize.h + 20 } 10 (toString n) [fill "#000000"] -- TODO 移動中の数え方
        , drawText { x = bpos.x + 0, y = bpos.y - 10 } 10 "+1" [fill "#000000", onClick (AddCookie bIdx 1)]
        , drawText { x = bpos.x + 30, y = bpos.y - 10 } 10 "+10" [fill "#000000", onClick (AddCookie bIdx 10)]
         , drawText { x = bpos.x + 70, y = bpos.y - 10 } 10 "-1" [fill "#000000", onClick (AddCookie bIdx -1)]
         , drawText { x = bpos.x + 100, y = bpos.y - 10 } 10 "-10" [fill "#000000", onClick (AddCookie bIdx -10)]
        , drawRect bpos bagSize [fill "#c9c44f" ]
        , drawLine bmt {bmt | y = bmt.y + bagSize.h} (strokeAttr 1 "#888888")
        ]
        ++ drawHoles bIdx cookiesPerBag
        ++ drawCookies bIdx n

guard : Bool -> Maybe a -> Maybe a
guard b m = case b of
    True -> m
    False -> Nothing

isNotEmpty : List a -> Bool
isNotEmpty = not << List.isEmpty


validate : Model -> Maybe Model
validate model =
    if model.bagL < 0 then Nothing
    else if model.bagL > cookiesPerBag then Nothing
    else if model.bagR < 0 then Nothing
    else if model.bagR > cookiesPerBag then Nothing
    else if isNotEmpty model.movingLR && isNotEmpty model.movingRL then Nothing
    else Just model


updateProgress : Time -> MovingCookie -> MovingCookie
updateProgress now mc = case mc.issuedAt of
    Just t  -> {mc | progress = (now - t) / movingTimeSec}
    Nothing -> {mc | issuedAt = Just now}


updateMovingCookies : Time -> Model -> Model
updateMovingCookies now model =
    let
        movingLR_ = List.map (updateProgress now) model.movingLR
        movingRL_ = List.map (updateProgress now) model.movingRL
        completesLR   = List.filter (\mv -> mv.progress >= 1) movingLR_
        imcompletesLR = List.filter (\mv -> mv.progress <  1) movingLR_
        completesRL   = List.filter (\mv -> mv.progress >= 1) movingRL_
        imcompletesRL = List.filter (\mv -> mv.progress <  1) movingRL_
    in
        {model
        | bagL = model.bagL + List.length completesRL
        , bagR = model.bagR + List.length completesLR
        , movingLR = imcompletesLR
        , movingRL = imcompletesRL
        }

-- TODO guard する時に移動先に空きがあること
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        model_ = Maybe.withDefault model <| case msg of
            Tick t -> Just <| updateMovingCookies t model
            AddCookie BagL n -> guard (List.isEmpty model.movingLR && List.isEmpty model.movingRL) <| validate {model | bagL = model.bagL + n}
            AddCookie BagR n -> guard (List.isEmpty model.movingLR && List.isEmpty model.movingRL) <| validate {model | bagR = model.bagR + n}
            MoveLRCookie -> guard (List.isEmpty model.movingRL && model.bagR < cookiesPerBag) <| validate {model | bagL = model.bagL - 1, movingLR = model.movingLR ++ [MovingCookie (model.bagL - 1) (countCookies BagR model) 0 Nothing]}
            MoveRLCookie -> guard (List.isEmpty model.movingLR && model.bagL < cookiesPerBag) <| validate {model | bagR = model.bagR - 1, movingRL = model.movingRL ++ [MovingCookie (model.bagR - 1) (countCookies BagL model) 0 Nothing]}
            _ -> Nothing
    in
        (model_, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
        Sub.batch
        [ every (33 * millisecond) Tick ]

