module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg
import Svg.Attributes
import Svg.Events

-- Model

type alias Model =
    { board : Array (Array Char)
    , wordsToFind : List (Bool, WordR)
    , mouseDrag : Maybe (Position, Position, Position)
    }

type alias WordR =
    { word : String
    , description : String
    , byDescription : Bool
    , start : Position
    , end : Position
    }

type alias Position = ( Int, Int )

-- Initial Model

initialModel : Model
initialModel =
    { board = Array.fromList [ Array.fromList [ 'E', 'L', 'M', 'A' ]
                             , Array.fromList [ 'A', 'R', 'T', 'S' ]
                             , Array.fromList [ 'T', 'E', 'S', 'T' ]
                             , Array.fromList [ 'S', 'E', 'A', 'L' ]
                             ]
    , wordsToFind = [ { word = "ELMA", description = "Personal name", byDescription = False, start = (0,0), end = (3,0) }
                    , { word = "ARTS", description = "", byDescription = False, start = (0,1), end = (3,1) }
                    , { word = "TEST", description = "Examination", byDescription = True, start = (0,2), end = (3,2) }
                    , { word = "SEAL", description = "", byDescription = False, start = (0,3), end = (3,3) }
                    , { word = "EATS", description = "Consumes", byDescription = True, start = (0,0), end = (0,3) }
                    ] |> List.map (\w -> (False, w))
    , mouseDrag = Nothing
    }

-- Update

type Msg
    = MouseDown Position
    | MouseUp
    | MouseOver Position
    | MouseOut

update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown pos -> { model | mouseDrag = Just (pos, pos, pos) }
        MouseUp -> case model.mouseDrag of
            Nothing -> model
            Just (mds, mde, _) -> case updateWords mds mde model.wordsToFind of
                Just words -> { model | wordsToFind = words, mouseDrag = Nothing }
                Nothing -> { model | mouseDrag = Nothing }
        MouseOver pos -> case model.mouseDrag of
            Nothing -> model
            Just (mds, _, _) -> { model | mouseDrag = Just (mds, ortoDiaLock mds pos, pos) }
        MouseOut -> model

updateWords : Position -> Position -> List (Bool, WordR) -> Maybe (List (Bool, WordR))
updateWords start end words =
    let p e = (e.start == start && e.end == end) || (e.start == end && e.end == start) in
    if words |> List.any (\(_, a) -> p a) then
        Just (words |> List.map (\(s, e) -> if not s && p e then (True, e) else (s, e)))
    else Nothing

ortoDiaLock : Position -> Position -> Position
ortoDiaLock origin end =
    let ((ox, oy), (ex, ey)) = (origin, end) in
    if ox == ex || oy == ey || abs (ox - ex) == abs (oy - ey) then end
    else let ortP = if abs (ox - ex) <= abs (oy - ey) then (ox, ey) else (ex, oy) in
            ortP -- TODO: test for projection to diagonal

-- View

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewBoard model.board model.wordsToFind model.mouseDrag
        , viewWords model.wordsToFind
        ]

viewBoard : Array (Array Char) -> List (Bool, WordR) -> Maybe (Position, Position, Position) -> Html.Html Msg
viewBoard board words drag =
    let
        betweenP start end (x, y) =
            let ((sx, sy), (ex, ey)) = (start, end)
                ((mnx, mxx), (mny, mxy)) = ((min sx ex, max sx ex), (min sy ey, max sy ey)) in
                (abs (sx - x) == abs (sy - y) && x >= mnx && x <= mxx && y >= mny && y <= mxy)
                            || (sx == x && ex == x && y >= mny && y <= mxy)
                            || (sy == y && ey == y && x >= mnx && x <= mxx)

        selectedP = case drag of
            Just (origin, end, _) -> (\e -> betweenP origin end e)
            Nothing -> (\e -> False)

        linePosition start end =
            let ((sx, sy), (ex, ey)) = (start, end) in
                ((sx * 30 + 15, sy * 30 + 15), (ex * 30 + 15, ey * 30 + 15))

        renderChar rowIndex colIndex char =
            let (x, y) = (colIndex * 30, rowIndex * 30) in
            Svg.node "text" [ Svg.Attributes.x (String.fromInt (x + 15))
                      , Svg.Attributes.y (String.fromInt (y + 20))
                      , Svg.Attributes.style ("text-anchor: middle;"
                                             ++ "font-size: 20px;"
                                             ++ "user-select: none;"
                                             ++ "color:" ++ (if selectedP (rowIndex, colIndex) then "red" else "black"))
                      , Svg.Events.onMouseDown <| MouseDown <| (colIndex, rowIndex)
                      , Svg.Events.onMouseOver <| MouseOver <| (colIndex, rowIndex)
                      ]
                      [ Svg.text <| String.fromChar char ]

        renderStrikeThrough : WordR -> Svg.Svg Msg
        renderStrikeThrough word =
            let ((x1, y1), (x2, y2)) = linePosition word.start word.end
            in
            Svg.line [ Svg.Attributes.x1 (String.fromInt x1)
                      , Svg.Attributes.y1 (String.fromInt y1)
                      , Svg.Attributes.x2 (String.fromInt x2)
                      , Svg.Attributes.y2 (String.fromInt y2)
                      , Svg.Attributes.stroke "#FF0000"
                      , Svg.Attributes.strokeOpacity "25%"
                      , Svg.Attributes.strokeWidth "3"
                      ] []
        renderDrag : Position -> Position -> Svg.Svg Msg
        renderDrag start end =
            let ((x1, y1), (x2, y2)) = linePosition start end
            in
            Svg.line [ Svg.Attributes.x1 (String.fromInt x1)
                      , Svg.Attributes.y1 (String.fromInt y1)
                      , Svg.Attributes.x2 (String.fromInt x2)
                      , Svg.Attributes.y2 (String.fromInt y2)
                      , Svg.Attributes.stroke "#FF0000"
                      , Svg.Attributes.strokeOpacity "75%"
                      , Svg.Attributes.strokeWidth "3"
                      ] []

    in
        Svg.svg [ Svg.Attributes.width (board |> Array.get 0 |> Maybe.map Array.length |> Maybe.withDefault 0 |> (*) 30 |> String.fromInt)
                , Svg.Attributes.height (board |> Array.length |> (*) 30 |> String.fromInt)
                , Svg.Events.onMouseOut MouseOut
                , Svg.Events.onMouseUp MouseUp
                ]
            ((case drag of
                    Nothing -> []
                    Just (start, end, _) -> [renderDrag start end])
                ++ (words |> List.filter Tuple.first |> List.map (Tuple.second >> renderStrikeThrough))
                ++ (board |> Array.indexedMap (\rowIndex row -> (row |> Array.indexedMap (renderChar rowIndex) |> Array.toList)) |> Array.toList |> List.concat))

viewWords : List (Bool, WordR) -> Html.Html Msg
viewWords words =
    Html.ul []
        (words |> List.map (\(s, word) ->
            Html.li [ Html.Attributes.style "text-decoration" (if s then "line-through" else "none")
                    , (if word.byDescription then (Html.Attributes.style "font-style" "italic")
                                             else (Html.Attributes.style "font-weight" "bold"))
                    , Html.Attributes.title word.description
                    ]
                    [ Html.text (if not word.byDescription then word.word else
                        (if not s then word.description else  word.word ++ " (" ++ word.description ++ ")" )) ])
        )

-- Main

main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }
