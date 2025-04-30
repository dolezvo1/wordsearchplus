module Main exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg
import Svg.Attributes
import Svg.Events
import Http
import Json.Decode as Decode

-- Helper

listAt : Int -> List a -> Maybe a
listAt idx lst = lst |> List.drop idx |> List.head

listSet : Int -> a -> List a -> List a
listSet idx elm lst = (lst |> List.take idx) ++ [elm] ++ (lst |> List.drop (idx+1))

-- Main

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

-- Model

type alias Model =
    { newGameWindow: NewGameWindowModel
    , dictSources: List DictSource
    , board : Array (Array Char)
    , wordsToFind : List (Bool, WordR)
    , mouseDrag : Maybe (Position, Position)
    }

type alias DictSource =
    { name: String
    , url: String
    , content: Maybe (Dict String String)
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

type alias NewGameWindowModel =
    { show: Bool
    , dictionaryIndex: Maybe Int
    , newDictName: String
    , newDictUrl: String
    , generateAllDirections: Bool
    , allowSearchByWords: Bool
    , allowSearchByDefinitions: Bool
    }

init : () -> (Model, Cmd Msg)
init () =
    ({ newGameWindow =
        { show = False
        , dictionaryIndex = Nothing
        , newDictName = ""
        , newDictUrl = ""
        , generateAllDirections = True
        , allowSearchByWords = True
        , allowSearchByDefinitions = True
        }
    , dictSources =
      [
        -- https://gist.github.com/BideoWego/60fbd40d5d1f0f1beca11ba95221dd38
        { name = "English dictionary (~30K entries)"
        , url = "https://gist.githubusercontent.com/BideoWego/60fbd40d5d1f0f1beca11ba95221dd38/raw/58fb4cce910fbf5fa67a2f0f1f619c09d7b1b373/dictionary.json"
        , content = Nothing
        }
        -- https://github.com/hathibelagal/German-English-JSON-Dictionary/blob/master/german_english.json
      , { name = "German-English dictionary (~4.5K entries)"
        , url = "https://raw.githubusercontent.com/hathibelagal/German-English-JSON-Dictionary/refs/heads/master/german_english.json"
        , content = Nothing
        }
      ]
    , board = Array.fromList [ Array.fromList [ 'E', 'L', 'M', 'A' ]
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
    }, Cmd.none)

fetchDictSource : (Maybe NewGameWindowModel) -> (Int, DictSource) -> Cmd Msg
fetchDictSource mngwm (idx, ds) = case ds.content of
    Just _ -> Cmd.none
    Nothing -> Http.get
        { url = ds.url
        , expect = Http.expectJson (Result.map (\e -> {ds | content = Just e}) >> GotSource mngwm idx) (Decode.dict Decode.string)
        }


-- Update

type Msg
    = NGWM NewGameWindowMsg
    | GotSource (Maybe NewGameWindowModel) Int (Result Http.Error DictSource)
    | AddNewDict
    | NewGame NewGameWindowModel
    | WordListReveal
    | WordListCollapse
    | MouseDown Position
    | MouseUp
    | MouseOver Position
    | MouseOut

type NewGameWindowMsg
    = SetVisibility Bool
    | SetCurrentDict (Maybe Int)
    | SetNewDictName String
    | SetNewDictUrl String
    | SetGenerateAllDirections Bool
    | SetAllowSearchByWords Bool
    | SetAllowSearchByDefinitions Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let cmdNone m = (m, Cmd.none)
        setGameBoard m ngwm = case ngwm.dictionaryIndex of
            Nothing -> Ok m
            Just dsi -> case listAt dsi m.dictSources of
                Nothing -> Ok m
                Just ds -> case ds.content of
                    Nothing -> Err ds
                    Just c -> let newBoard = m.board -- TODO: word selection, board generation
                              in Ok {m | board = newBoard}
    in
    case msg of
        NGWM msg2 ->
            let oldNewGameWindow = model.newGameWindow
                newNewGameWindow = case msg2 of
                    SetVisibility b -> { oldNewGameWindow | show = b }
                    SetCurrentDict mi -> { oldNewGameWindow | dictionaryIndex = mi }
                    SetNewDictName s -> { oldNewGameWindow | newDictName = s }
                    SetNewDictUrl s -> { oldNewGameWindow | newDictUrl = s }
                    SetGenerateAllDirections b -> { oldNewGameWindow | generateAllDirections = b }
                    SetAllowSearchByWords b -> { oldNewGameWindow | allowSearchByWords = b }
                    SetAllowSearchByDefinitions b -> { oldNewGameWindow | allowSearchByDefinitions = b }
            in { model | newGameWindow = newNewGameWindow } |> cmdNone
        GotSource mngwm idx rds -> cmdNone <| case rds of
            Err _ -> model
            Ok ds -> let newDictSources = listSet idx ds model.dictSources
                         m2 = { model | dictSources = newDictSources } in
                case mngwm of
                    Nothing -> m2
                    Just ngwm -> case setGameBoard m2 ngwm of
                        Err _ -> m2
                        Ok m3 -> m3
        AddNewDict ->
            let
                newDictSource = DictSource model.newGameWindow.newDictName model.newGameWindow.newDictUrl Nothing
                newDictSources = model.dictSources ++ [newDictSource]
            in cmdNone { model | dictSources = newDictSources }
        NewGame ngwm -> case ngwm.dictionaryIndex of
            Nothing -> cmdNone model
            Just idx -> case setGameBoard model ngwm of
                Err ds -> (model, fetchDictSource (Just ngwm) (idx, ds))
                Ok m -> cmdNone m
        WordListReveal -> cmdNone model
        WordListCollapse -> cmdNone model
        MouseDown pos -> cmdNone { model | mouseDrag = Just (pos, pos) }
        MouseUp ->
            let tryFindWord : (Position, Position) -> List (Bool, WordR) -> Maybe (List (Bool, WordR))
                tryFindWord (start, end) words =
                    let p e = (e.start == start && e.end == end) || (e.start == end && e.end == start) in
                    if words |> List.any (\(_, a) -> p a) then
                        Just (words |> List.map (\(s, e) -> if not s && p e then (True, e) else (s, e)))
                    else Nothing
            in cmdNone <| case model.mouseDrag of
                Nothing -> model
                Just (mds, mde) -> case tryFindWord (mds, mde) model.wordsToFind of
                    Just words -> { model | wordsToFind = words, mouseDrag = Nothing }
                    Nothing -> { model | mouseDrag = Nothing }
        MouseOver pos ->
            let positionOrtoDiaLock : Position -> Position -> Position
                positionOrtoDiaLock origin end =
                    let ((ox, oy), (ex, ey)) = (origin, end) in
                    if ox == ex || oy == ey || abs (ox - ex) == abs (oy - ey) then end
                    else let ortP = if abs (ox - ex) <= abs (oy - ey) then (ox, ey) else (ex, oy) in
                            ortP -- TODO: test for projection to diagonal
            in cmdNone <| case model.mouseDrag of
                Nothing -> model
                Just (mds, _) -> { model | mouseDrag = Just (mds, positionOrtoDiaLock mds pos) }
        MouseOut -> cmdNone model

-- View

view : Model -> Html Msg
view model =
    Html.div [] ([
        Html.div []
        [ Html.button [ Html.Events.onClick <| NGWM <| SetVisibility True ] [ Html.text "New Game" ]
        -- TODO: hide when not applicable
        , Html.button [ Html.Events.onClick WordListReveal ] [ Html.text "Reveal exact words" ]
        -- TODO: icon
        , Html.button [ Html.Events.onClick WordListCollapse ] [ Html.text "Collapse" ]
        ],
        Html.div []
        [ viewBoard model.board model.wordsToFind model.mouseDrag
        , viewWords model.wordsToFind
        ]
    ] ++ (viewNewGameWindow model))

viewNewGameWindow : Model -> List (Html Msg)
viewNewGameWindow model =
    let viewDictSource i ds = Html.label [ Html.Attributes.title ds.url ] [
            Html.input [ Html.Attributes.type_ "radio"
                       , Html.Attributes.name "dictSource"
                       , Html.Events.onClick <| NGWM <| SetCurrentDict <| Just <| i
                       , Html.Attributes.checked (case model.newGameWindow.dictionaryIndex of
                                                    Just i2 -> i == i2
                                                    Nothing -> False) ] []
            , Html.text ds.name ]
        chbx lbl s m = Html.label [] [ Html.input [ Html.Attributes.type_ "checkbox", Html.Events.onClick <| m <| not s, Html.Attributes.checked s ] [], Html.text lbl ]
    in if model.newGameWindow.show then [Html.div []
        [
            Html.div [] [
                Html.h4 [] [ Html.text "Select a dictionary:" ],
                Html.fieldset [] (model.dictSources |> List.indexedMap (\i e -> [viewDictSource i e, Html.br [] []]) |> List.concat),
                Html.div [] [
                    Html.input [ Html.Events.onInput <| NGWM << SetNewDictName ] [], Html.br [] [],
                    Html.input [ Html.Events.onInput <| NGWM << SetNewDictUrl ] [], Html.br [] [],
                    Html.input [ Html.Events.onClick <| AddNewDict, Html.Attributes.type_ "button", Html.Attributes.value "Add custom dictionary" ] []
                ]
            ]
        ,   Html.div [] [
                Html.h4 [] [ Html.text "Select options:" ],
                Html.div [] [
                    chbx "Generate words in all directions" model.newGameWindow.generateAllDirections (SetGenerateAllDirections >> NGWM), Html.br [] [],
                    chbx "Allow search by words" model.newGameWindow.allowSearchByWords (SetAllowSearchByWords >> NGWM), Html.br [] [],
                    chbx "Allow search by definitions" model.newGameWindow.allowSearchByDefinitions (SetAllowSearchByDefinitions >> NGWM), Html.br [] []
                ]
            ]
        ,   Html.input [ Html.Attributes.type_ "button", Html.Events.onClick <| NewGame <| model.newGameWindow, Html.Attributes.value "New Game" ] []
        ,   Html.input [ Html.Attributes.type_ "button", Html.Events.onClick <| NGWM <| SetVisibility False, Html.Attributes.value "Cancel" ] []
        ]] else []


viewBoard : Array (Array Char) -> List (Bool, WordR) -> Maybe (Position, Position) -> Html.Html Msg
viewBoard board words drag =
    let
        betweenP start end (x, y) =
            let ((sx, sy), (ex, ey)) = (start, end)
                ((mnx, mxx), (mny, mxy)) = ((min sx ex, max sx ex), (min sy ey, max sy ey)) in
                (abs (sx - x) == abs (sy - y) && x >= mnx && x <= mxx && y >= mny && y <= mxy)
                            || (sx == x && ex == x && y >= mny && y <= mxy)
                            || (sy == y && ey == y && x >= mnx && x <= mxx)

        selectedP = case drag of
            Just (origin, end) -> (\e -> betweenP origin end e)
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
                    Just (start, end) -> [renderDrag start end])
                ++ (words |> List.filter Tuple.first |> List.map (Tuple.second >> renderStrikeThrough))
                ++ (board |> Array.indexedMap (\rowIndex row -> (row |> Array.indexedMap (renderChar rowIndex) |> Array.toList)) |> Array.toList |> List.concat))

viewWords : List (Bool, WordR) -> Html.Html Msg
viewWords words =
    let foundWords = words |> List.filter Tuple.first |> List.length
        totalWords = words |> List.length
        percentageFound = 100 * toFloat foundWords / toFloat totalWords
    in
    Html.div [] [
        Html.h4 [] [ Html.text ("Found: " ++ (String.fromInt foundWords) ++ "/" ++ (String.fromInt totalWords) ++ " (" ++ (String.fromFloat percentageFound) ++ "%)")],
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
    ]
