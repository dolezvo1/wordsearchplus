module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events

-- Model

type alias Model =
    { board : Array (Array Char)
    , wordsToFind : List (Bool, WordR)
    , mouseDrag : Maybe (Position, Position, Position)
    }

type alias WordR =
    { word : String
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
    , wordsToFind = [ {word = "ELMA", start = (0,0), end = (3,0)}
                    , {word = "ARTS", start = (0,1), end = (3,1)}
                    , {word = "TEST", start = (0,2), end = (3,2)}
                    , {word = "SEAL", start = (0,3), end = (3,3)}
                    , {word = "EATS", start = (0,0), end = (0,3)}
                    ] |> List.map (\w -> (False, w))
    , mouseDrag = Nothing
    }

-- Update

type Msg
    = MouseDown Position
    | MouseUp
    | MouseOver Position

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
        foundP (x, y) = words |> List.any (\(s, w) -> s && betweenP w.start w.end (x, y))
    in
        Html.div []
        (board |> Array.indexedMap (\colIndex row ->
                Html.div []
                    (row |> Array.indexedMap (\rowIndex char ->
                            Html.span [ Html.Attributes.style "padding" "5px"
                                 , Html.Attributes.style "user-select" "none"
                                 , Html.Attributes.style "border" (if selectedP (rowIndex, colIndex) then "1px solid red" else "none")
                                 , Html.Attributes.style "background-color" (if foundP (rowIndex, colIndex) then "gray" else "none")
                                 , Html.Events.onMouseDown <| MouseDown <| (rowIndex, colIndex)
                                 , Html.Events.onMouseUp MouseUp
                                 , Html.Events.onMouseOver <| MouseOver <| (rowIndex, colIndex)
                                 ]
                                 [ Html.text (String.fromChar char) ]
                        )
                        |> Array.toList
                    )
            ) |> Array.toList
        )

viewWords : List (Bool, WordR) -> Html.Html Msg
viewWords words =
    Html.ul []
        (words |> List.map (\(s, word) ->
                Html.li [ Html.Attributes.style "text-decoration" (if s then "line-through" else "none") ]
                    [ Html.text word.word ])
        )

-- Main

main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }
