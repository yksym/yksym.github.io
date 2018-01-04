module DrawUtil exposing (..)
import Svg exposing (Svg, rect, circle, text_, line, text)
import Svg.Attributes exposing (..)

type alias Color = String -- "#00000f" how to derive Show??
type alias PositionF = { x: Float, y: Float}
type alias RectSizeF = { w: Float, h: Float}

strokeAttr : Int -> Color -> List (Svg.Attribute msg)
strokeAttr w color =
    [ w |> toString |> strokeWidth
    , stroke color]

drawCircle : PositionF -> Float -> List (Svg.Attribute msg) -> Svg msg
drawCircle pos r_ attrs = circle (
              [ pos.x |> floor |> toString |> cx
              , pos.y |> floor |> toString |> cy
              , r_ |> ceiling |> toString |> r
              ] ++ attrs) []

drawRect : PositionF -> RectSizeF -> List (Svg.Attribute msg) -> Svg msg
drawRect pos sz attrs = rect (
              [ pos.x |> floor |> toString |> x
              , pos.y |> floor |> toString |> y
              , sz.w |> ceiling |> toString |> width
              , sz.h |> ceiling |> toString |> height
              ] ++ attrs) []

drawText : PositionF -> Int -> String -> List (Svg.Attribute msg) -> Svg msg
drawText pos f_ s attrs = text_ (
              [ pos.x |> floor |> toString |> x
              , pos.y |> floor |> toString |> y
              , f_ |> toString |> fontSize
              ] ++ attrs) [text s]

drawLine : PositionF -> PositionF -> List (Svg.Attribute msg) -> Svg msg
drawLine pos1 pos2 attrs = line (
            [ pos1.x |> floor |> toString |> x1
            , pos1.y |> floor |> toString |> y1
            , pos2.x |> floor |> toString |> x2
            , pos2.y |> floor |> toString |> y2
            ] ++ attrs) []

bezier3 : Float -> Float -> Float -> Float -> Float
bezier3 p0 p1 p2 t = (1-t)*(1-t)*p0 + 2*(1-t)*t*p1 + t*t*p2

interp : PositionF -> PositionF -> PositionF -> Float -> PositionF
interp p0 p1 p2 t =
        { x = bezier3 p0.x p1.x p2.x t
        , y = bezier3 p0.y p1.y p2.y t
        }


