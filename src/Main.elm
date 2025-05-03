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
import Random
import List.Extra
import File
import File.Select
import Task

import BoardGenerator exposing (WordType(..))

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
    , board: Array (Array Char)
    , revealExactWords: Bool
    , wordsToFind: List (Bool, WordR)
    , mouseDrag: Maybe (Position, Position)
    , errorMessage: Maybe String
    }

type alias DictSource =
    { name: String
    , url: String
    , content: Maybe (Dict String String)
    }

type alias WordR =
    { word: String
    , description: String
    , wordType: WordType
    , start: Position
    , end: Position
    }

type alias Position = ( Int, Int )
type alias PositionF = ( Float, Float )

-- Initial Model

type alias NewGameWindowModel =
    { show: Bool
    , dictionaryIndex: Maybe Int
    , newDictName: String
    , newDictSourceRemote: Bool
    , newDictUrl: String
    , newDictFile: Maybe (String, Dict String String)
    , wordCount: Int
    , generateAllDirections: Bool
    , typeProbabilities: (Float, Float)
    , factor: Float
    }

init : () -> (Model, Cmd Msg)
init () =
    ({ newGameWindow =
        { show = False
        , dictionaryIndex = Nothing
        , newDictName = ""
        , newDictSourceRemote = True
        , newDictUrl = ""
        , newDictFile = Nothing
        , wordCount = 15
        , generateAllDirections = True
        , typeProbabilities = (0.5, 0.5)
        , factor = 0.5
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
    , revealExactWords = False
    , wordsToFind = [ { word = "ELM", description = "A purely functional programming language for the front-end", wordType = Word, start = (0,0), end = (2,0) }
                    , { word = "TEST", description = "Examination", wordType = Description, start = (0,2), end = (3,2) }
                    , { word = "SEAL", description = "A device for creation of impressions into wax or similar medium", wordType = None, start = (0,3), end = (3,3) }
                    , { word = "EATS", description = "Consumes", wordType = Description, start = (0,0), end = (0,3) }
                    , { word = "ARTS", description = "", wordType = Word, start = (0,1), end = (3,1) }
                    , { word = "SET", description = "A collection of unique elements", wordType = Description, start = (0,3), end = (2,1) }
                    ] |> List.map (\w -> (False, w))
    , mouseDrag = Nothing
    , errorMessage = Nothing
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
    | RequestedDictFileDialog
    | GotDictFile File.File
    | GotDictFileContent File.File String
    | AddNewDict
    | GotSource (Maybe NewGameWindowModel) Int (Result Http.Error DictSource)
    | NewGame NewGameWindowModel
    | GotGeneratedBoard (Result String (List (BoardGenerator.WordType, (String, String), ((Int, Int), (Int, Int))), (Array (Array Char))))
    | ClearErrorMessage
    | WordListReveal
    | WordListCollapse
    | MouseDown Position
    | MouseUp
    | MouseOver Position

type NewGameWindowMsg
    = SetVisibility Bool
    | SetCurrentDict (Maybe Int)
    | SetNewDictName String
    | SetDictSourceType Bool
    | SetNewDictUrl String
    | SetWordCount Int
    | SetGenerateAllDirections Bool
    | SetTypeProbabilities (Float, Float)
    | SetFactor Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let cmdNone m = (m, Cmd.none)
        setGameBoard m = case m.newGameWindow.dictionaryIndex of
            Nothing -> cmdNone m
            Just dsi -> case List.Extra.getAt dsi m.dictSources of
                Nothing -> cmdNone m
                Just ds -> case ds.content of
                    Nothing -> (m, fetchDictSource (Just m.newGameWindow) (dsi, ds))
                    Just c -> (m, BoardGenerator.generateWordSearch m.newGameWindow.wordCount m.newGameWindow.typeProbabilities c m.newGameWindow.generateAllDirections m.newGameWindow.factor |> Random.generate GotGeneratedBoard)
    in
    case msg of
        NGWM msg2 ->
            let oldNewGameWindow = model.newGameWindow
                newNewGameWindow = case msg2 of
                    SetVisibility b -> { oldNewGameWindow | show = b }
                    SetCurrentDict mi -> { oldNewGameWindow | dictionaryIndex = mi }
                    SetNewDictName s -> { oldNewGameWindow | newDictName = s }
                    SetDictSourceType b -> { oldNewGameWindow | newDictSourceRemote = b}
                    SetNewDictUrl s -> { oldNewGameWindow | newDictUrl = s }
                    SetWordCount i -> { oldNewGameWindow | wordCount = i }
                    SetGenerateAllDirections b -> { oldNewGameWindow | generateAllDirections = b }
                    SetTypeProbabilities p -> { oldNewGameWindow | typeProbabilities = p }
                    SetFactor p -> { oldNewGameWindow | factor = p }
            in { model | newGameWindow = newNewGameWindow } |> cmdNone
        RequestedDictFileDialog -> (model, File.Select.file [] GotDictFile)
        GotDictFile file -> (model, Task.perform (GotDictFileContent <| file) (File.toString file))
        GotDictFileContent file fileContent ->
            cmdNone <| case Decode.decodeString (Decode.dict Decode.string) fileContent of
                Err e -> { model | errorMessage = Just <| Decode.errorToString <| e }
                Ok dict ->
                    let oldNewGameWindow = model.newGameWindow
                        newNewGameWindow = { oldNewGameWindow | newDictFile = Just (File.name file, dict) }
                    in { model | newGameWindow = newNewGameWindow }
        AddNewDict ->
            let ongw = model.newGameWindow
                newDictSource = (DictSource ongw.newDictName
                    (if ongw.newDictSourceRemote then ongw.newDictUrl else "")
                    (ongw.newDictFile |> Maybe.map (\(n, c) -> c)))
            in if newDictSource.url == "" && (newDictSource.content |> Maybe.map (\e -> False) |> Maybe.withDefault True) then
                cmdNone model
                else let newDictSources = model.dictSources ++ [newDictSource]
                         newNewGameWindow = { ongw | newDictName = "", newDictUrl = "", newDictFile = Nothing }
                    in cmdNone { model | dictSources = newDictSources, newGameWindow = newNewGameWindow }
        GotSource mngwm idx rds -> case rds of
            Err e -> let errorText = (case e of -- why???
                                        Http.BadUrl s -> "Bad URL: " ++ s
                                        Http.Timeout -> "Request Timeout"
                                        Http.NetworkError -> "Network Error"
                                        Http.BadStatus i -> "Bad HTTP Status: " ++ String.fromInt i
                                        Http.BadBody s -> "Bad HTTP body: " ++ s)
                in cmdNone { model | errorMessage = Just errorText }
            Ok ds -> let newDictSources = List.Extra.setAt idx ds model.dictSources
                         m2 = { model | dictSources = newDictSources } in
                case mngwm of
                    Nothing -> cmdNone m2
                    Just ngwm -> setGameBoard m2
        GotGeneratedBoard res -> cmdNone <| case res of
            Err e -> { model | errorMessage = Just e }
            Ok (newWords, newBoard) ->
                let newWordsToFind = newWords |> List.map (\(wt, (w, d), (sp, ep)) -> (False, { word=w, description=d, wordType=wt, start=sp, end=ep }))
                in { model | board = newBoard, wordsToFind = newWordsToFind, revealExactWords = False }
        NewGame ngwm -> case ngwm.dictionaryIndex of
            Nothing -> cmdNone model
            Just idx -> let newNGWM = { ngwm | show = False } in setGameBoard { model | newGameWindow = newNGWM }
        ClearErrorMessage -> cmdNone { model | errorMessage = Nothing }
        WordListReveal -> cmdNone { model | revealExactWords = True }
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
                    else
                        let hypot a b = sqrt (toFloat (a*a) + toFloat (b*b))
                            (boardW, boardH) = (model.board |> Array.get 0 |> Maybe.map Array.length |> Maybe.withDefault 0, model.board |> Array.length)
                            (dx, dy) = (ex - ox, ey - oy)
                        in [(ox, ey), (ex, oy),
                            (ox + dx, oy + dx), (ox + dy, oy + dy), (ox - dx, oy - dx), (ox - dy, oy - dy),
                            (ox - dx, oy + dx), (ox - dy, oy + dy), (ox + dx, oy - dx), (ox + dy, oy - dy)]
                                |> List.filter (\(cx, cy) -> cx >= 0 && cx < boardW && cy >= 0 && cy < boardH)
                                |> List.map (\(cx, cy) -> (clamp 0 boardW cx, clamp 0 boardH cy))
                                |> List.map (\(cx, cy) -> (hypot (ex - cx) (ey - cy), (cx, cy)))
                                |> List.sort
                                |> List.head |> Maybe.map (\(_, p) -> p) |> Maybe.withDefault (ex, ey)

            in cmdNone <| case model.mouseDrag of
                Nothing -> model
                Just (mds, _) -> { model | mouseDrag = Just (mds, positionOrtoDiaLock mds pos) }

-- View

view : Model -> Html Msg
view model =
    Html.div [] ([
        Html.div []
        ([ Html.button [ Html.Events.onClick <| NGWM <| SetVisibility True ] [ Html.text "New Game" ]]
         ++ (if not model.revealExactWords then [ Html.button [ Html.Events.onClick WordListReveal ] [ Html.text "Reveal exact words" ] ] else [])),
        Html.div [ Html.Attributes.style "display" "flex" ]
        [ viewBoard model.board model.wordsToFind model.mouseDrag
        , viewWords model.wordsToFind model.revealExactWords
        ]
    ] ++ (viewNewGameWindow model)
      ++ (viewErrorMessage model))

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
        sToFP = String.toFloat >> Maybe.withDefault 0 >> (\e -> e / 100)
        fpToS = (*) 100 >> String.fromFloat
    in if model.newGameWindow.show then [
    Html.div [ Html.Attributes.style "position" "fixed"
             , Html.Attributes.style "top" "0"
             , Html.Attributes.style "left" "0"
             , Html.Attributes.style "width" "100%"
             , Html.Attributes.style "height" "100%"
             , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.75)"
             ] [
        Html.div [ Html.Attributes.style "width" "90%"
                 , Html.Attributes.style "max-width" "500px"
                 , Html.Attributes.style "margin" "auto auto"
                 , Html.Attributes.style "background-color" "white"
                 , Html.Attributes.style "padding" "10px"
                 , Html.Attributes.style "display" "flex"
                 , Html.Attributes.style "flex-direction" "column" ]
        [
            Html.div [] [
                Html.h4 [] [ Html.text "Select a dictionary:" ],
                Html.fieldset [] (model.dictSources |> List.indexedMap (\i e -> [viewDictSource i e, Html.br [] []]) |> List.concat),
                Html.details [ Html.Attributes.style "border" "1px solid gray"
                             , Html.Attributes.style "padding" "10px"] [
                    Html.summary [] [ Html.text "Add custom dictionary:" ],

                    Html.label [] [
                            Html.input [ Html.Events.onInput <| NGWM << SetNewDictName
                                       , Html.Attributes.value <| model.newGameWindow.newDictName ] []
                          , Html.text "Display name" ], Html.br [] [],
                    Html.fieldset [] [
                        Html.label [] [
                            Html.input [ Html.Attributes.type_ "radio"
                                       , Html.Events.onInput <| (\_ -> NGWM <| SetDictSourceType <| True)
                                       , Html.Attributes.name "newDictSourceType"
                                       , Html.Attributes.checked <| model.newGameWindow.newDictSourceRemote ] []
                            , Html.input [ Html.Events.onInput <| NGWM << SetNewDictUrl
                                        , Html.Attributes.value <| model.newGameWindow.newDictUrl ] []
                            , Html.text "Remote URL" ], Html.br [] [],
                        Html.label [] [
                            Html.input [ Html.Attributes.type_ "radio"
                                       , Html.Events.onInput <| (\_ -> NGWM <| SetDictSourceType <| False)
                                       , Html.Attributes.name "newDictSourceType"
                                       , Html.Attributes.checked <| not model.newGameWindow.newDictSourceRemote ] []
                            , Html.button [ Html.Events.onClick <| RequestedDictFileDialog ] [
                                    Html.text (case model.newGameWindow.newDictFile of
                                                    Nothing -> "Select a local file"
                                                    Just (fileName, _) -> fileName) ]
                            , Html.text "Local file" ], Html.br [] []
                    ],
                    Html.input [ Html.Events.onClick <| AddNewDict, Html.Attributes.type_ "button", Html.Attributes.value "Add custom dictionary" ] []
                ]
            ]
        ,   Html.div [ Html.Attributes.style "margin-top" "10px" ] [
                Html.h4 [] [ Html.text "Select game options:" ],
                Html.div [] [
                      Html.label [] [
                            Html.input [ Html.Attributes.type_ "number"
                                       , Html.Events.onInput <| (NGWM << SetWordCount << Maybe.withDefault 0 << String.toInt)
                                       , Html.Attributes.value <| String.fromInt <| model.newGameWindow.wordCount
                                       , Html.Attributes.min "1"
                            ] []
                          , Html.text "Number of words" ], Html.br [] []
                    , Html.label [ Html.Attributes.title "Only generates words Forward, Down and ForwardDown when unselected" ] [
                              Html.input [ Html.Attributes.type_ "checkbox"
                            , Html.Events.onClick <| (NGWM << SetGenerateAllDirections) <| not model.newGameWindow.generateAllDirections
                            , Html.Attributes.checked model.newGameWindow.generateAllDirections ] []
                        , Html.text "Generate words in all directions" ], Html.br [] []
                    , Html.label [ Html.Attributes.title "0 = never exact words, 100 = always exact words" ] [
                            Html.input [ Html.Attributes.type_ "range"
                                       , Html.Events.onInput <| (NGWM << SetTypeProbabilities << (\e -> (e, Tuple.second <| model.newGameWindow.typeProbabilities)) << sToFP)
                                       , Html.Attributes.value <| fpToS <| Tuple.first <| model.newGameWindow.typeProbabilities
                            ] []
                          , Html.text "Probability of search by exact word" ], Html.br [] []
                    , Html.label [ Html.Attributes.title "0 = never descriptions, 100 = always descriptions (when not exact words)" ] [
                            Html.input [ Html.Attributes.type_ "range"
                                       , Html.Events.onInput <| (NGWM << SetTypeProbabilities << (\e -> (Tuple.first <| model.newGameWindow.typeProbabilities, e)) << sToFP)
                                       , Html.Attributes.value <| fpToS <| Tuple.second <| model.newGameWindow.typeProbabilities
                            ] []
                          , Html.text "Probability of search by description" ], Html.br [] []
                    , Html.label [] [
                            Html.input [ Html.Attributes.type_ "range"
                                       , Html.Events.onInput <| (NGWM << SetFactor << (\e -> e / 100) << Maybe.withDefault 0 << String.toFloat)
                                       , Html.Attributes.value <| String.fromFloat <| (*) 100 <| model.newGameWindow.factor
                            ] []
                          , Html.text "Maximum density factor" ], Html.br [] []
                ]
            ]
        ,   Html.input [ Html.Attributes.type_ "button", Html.Events.onClick <| NewGame <| model.newGameWindow, Html.Attributes.value "New Game" ] []
        ,   Html.input [ Html.Attributes.type_ "button", Html.Events.onClick <| NGWM <| SetVisibility False, Html.Attributes.value "Cancel" ] []
        ]]] else []

viewErrorMessage : Model -> List (Html Msg)
viewErrorMessage model =
    case model.errorMessage of
        Nothing -> []
        Just errorMessage ->
            [
            Html.div [ Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.75)"
                    ] [
                Html.div [ Html.Attributes.style "width" "90%"
                        , Html.Attributes.style "max-width" "500px"
                        , Html.Attributes.style "margin" "auto auto"
                        , Html.Attributes.style "background-color" "white"
                        , Html.Attributes.style "padding" "10px"
                        , Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "column" ]
                [
                        Html.h4 [] [ Html.text "Error:" ],
                        Html.p [] [ Html.text errorMessage ],
                    Html.input [ Html.Attributes.type_ "button", Html.Events.onClick <| ClearErrorMessage, Html.Attributes.value "Okay" ] []
                ]]
            ]

viewBoard : Array (Array Char) -> List (Bool, WordR) -> Maybe (Position, Position) -> Html Msg
viewBoard board words drag =
    let
        (letterW, fontSize, padding) = (50, 37, 20)
        letterWH = letterW / 2
        (boardW, boardH) = (board |> Array.get 0 |> Maybe.map Array.length |> Maybe.withDefault 0, board |> Array.length)

        betweenP start end (x, y) =
            let ((sx, sy), (ex, ey)) = (start, end)
                ((mnx, mxx), (mny, mxy)) = ((min sx ex, max sx ex), (min sy ey, max sy ey)) in
                (abs (sx - x) == abs (sy - y) && x >= mnx && x <= mxx && y >= mny && y <= mxy)
                            || (sx == x && ex == x && y >= mny && y <= mxy)
                            || (sy == y && ey == y && x >= mnx && x <= mxx)

        selectedP = case drag of
            Just (origin, end) -> (\e -> betweenP origin end e)
            Nothing -> (\e -> False)

        linePosition : Position -> Position -> (PositionF, PositionF)
        linePosition start end =
            let ((sx, sy), (ex, ey)) = (start, end) in
                ( (toFloat sx * letterW + letterWH, toFloat sy * letterW + letterWH)
                , (toFloat ex * letterW + letterWH, toFloat ey * letterW + letterWH) )

        renderBoarders = [ Svg.rect [ Svg.Attributes.x <| String.fromFloat <| 0
                                    , Svg.Attributes.y <| String.fromFloat <| 0
                                    , Svg.Attributes.width <| String.fromInt <| (boardW * letterW)
                                    , Svg.Attributes.height <| String.fromInt <| (boardH * letterW)
                                    , Svg.Attributes.fillOpacity "0%"
                                    , Svg.Attributes.stroke "#000000"
                                    , Svg.Attributes.strokeWidth "2"
                                    ] []]
        renderChar rowIndex colIndex char =
            let (x, y) = (toFloat <| colIndex * letterW, toFloat <| rowIndex * letterW) in
            [ Svg.node "text" [ Svg.Attributes.x (String.fromFloat (x + letterWH))
                              , Svg.Attributes.y (String.fromFloat (y + fontSize))
                              , Svg.Attributes.style ("text-anchor: middle;"
                                                    ++ "font-size: " ++ String.fromInt fontSize ++ "px;"
                                                    ++ "user-select: none;"
                                                    ++ "color:" ++ (if selectedP (rowIndex, colIndex) then "red" else "black"))
                              ]
                              [ Svg.text <| String.fromChar char ]
            , Svg.rect [
                       Svg.Attributes.x <| String.fromFloat x
                     , Svg.Attributes.y <| String.fromFloat y
                     , Svg.Attributes.width <| String.fromInt letterW
                     , Svg.Attributes.height <| String.fromInt letterW
                     , Svg.Attributes.fillOpacity "0%"
                     , Svg.Events.onMouseDown <| MouseDown <| (colIndex, rowIndex)
                     , Svg.Events.onMouseOver <| MouseOver <| (colIndex, rowIndex)
                     ] []
            ]

        renderStrikeThrough word =
            let ((x1, y1), (x2, y2)) = linePosition word.start word.end
            in
            Svg.line [ Svg.Attributes.x1 (String.fromFloat x1)
                      , Svg.Attributes.y1 (String.fromFloat y1)
                      , Svg.Attributes.x2 (String.fromFloat x2)
                      , Svg.Attributes.y2 (String.fromFloat y2)
                      , Svg.Attributes.stroke "#FF0000"
                      , Svg.Attributes.strokeOpacity "25%"
                      , Svg.Attributes.strokeWidth "3"
                      ] []
        renderDrag : Position -> Position -> Svg.Svg Msg
        renderDrag start end =
            let ((x1, y1), (x2, y2)) = linePosition start end
                (cx, cy) = ((x1 + x2) / 2, (y1 + y2) / 2)
                length = (+) letterW <| sqrt <| (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)))
                angleDeg = (+) 90 <| (*) (-180/pi) <| atan2 (x2 - x1) (y2 - y1)
            in
                Svg.rect [ Svg.Attributes.x <| String.fromFloat <| (-length/2)
                        , Svg.Attributes.y <| String.fromFloat <| (-letterW/2)
                        , Svg.Attributes.width <| String.fromFloat <| length
                        , Svg.Attributes.height <| String.fromInt <| letterW
                        , Svg.Attributes.fillOpacity "0%"
                        , Svg.Attributes.rx <| String.fromFloat <| letterWH
                        , Svg.Attributes.ry <| String.fromFloat <| letterWH
                        , Svg.Attributes.transform ("translate(" ++ String.fromFloat cx ++ ", " ++ String.fromFloat cy ++ ") rotate(" ++ String.fromFloat angleDeg ++ ")")
                        , Svg.Attributes.stroke "#FF0000"
                        , Svg.Attributes.strokeWidth "3"
                        ] []

    in
        Svg.svg [
                  Svg.Attributes.style "max-height: calc(100vh - 50px);"
                , Svg.Attributes.width (boardW |> (*) letterW |> (+) (2*padding) |> String.fromInt)
                , Svg.Attributes.height (boardH |> (*) letterW |> (+) (2*padding) |> String.fromInt)
                , Svg.Events.onMouseUp MouseUp
                , Svg.Attributes.viewBox (String.fromInt -padding ++ " " ++ String.fromInt -padding
                                            ++ " " ++ String.fromInt (boardW * letterW + 2*padding) ++ " " ++ String.fromInt (boardH * letterW + 2*padding))
                ]
            (renderBoarders
                ++ (case drag of
                    Nothing -> []
                    Just (start, end) -> [renderDrag start end])
                ++ (words |> List.filter Tuple.first |> List.map (Tuple.second >> renderStrikeThrough))
                ++ (board |> Array.indexedMap (\rowIndex row -> (row |> Array.indexedMap (renderChar rowIndex) |> Array.toList |> List.concat)) |> Array.toList |> List.concat))

viewWords : List (Bool, WordR) -> Bool -> Html.Html Msg
viewWords words revealExactWords =
    let foundWords = words |> List.filter Tuple.first |> List.length
        totalWords = words |> List.length
        percentageFound = let factor = 100 in
                    (toFloat <| round <| (*) (100 * factor) <| toFloat foundWords / toFloat totalWords) / factor
    in
    Html.div [ Html.Attributes.style "width" "50%" ] [
        Html.h4 [] [ Html.text ("Found: " ++ (String.fromInt foundWords) ++ "/" ++ (String.fromInt totalWords) ++ " (" ++ (String.fromFloat percentageFound) ++ "%)")],
        Html.div [ Html.Attributes.style "overflow-y" "auto", Html.Attributes.style "max-height" "calc(100vh - 100px)" ] [
            Html.ul []
                (words |> List.map (\(s, word) ->
                    Html.li [ Html.Attributes.style "text-decoration" (if s then "line-through" else "none")
                            , (case word.wordType of
                                Word -> Html.Attributes.style "font-weight" "bold"
                                Description -> Html.Attributes.style "font-style" "italic"
                                None -> Html.Attributes.style "color" "red")
                            , Html.Attributes.title (if (word.wordType /= None) || s || revealExactWords then word.description else "???")
                            ]
                            [ Html.text (case word.wordType of
                                            Word -> word.word
                                            Description -> (if not s && not revealExactWords then word.description else  word.word ++ " (" ++ word.description ++ ")" )
                                            None -> if not s && not revealExactWords then "???" else word.word) ])
                )
        ]
    ]
