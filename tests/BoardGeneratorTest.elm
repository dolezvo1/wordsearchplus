-- File licensed under MIT


module BoardGeneratorTest exposing (..)

import Array exposing (Array)
import BoardGenerator exposing (..)
import Dict exposing (Dict)
import Expect exposing (..)
import Fuzz exposing (..)
import List.Extra
import Random exposing (Generator)
import Random.List
import Test exposing (..)


countWordOccurrences : String -> List (List Char) -> Int
countWordOccurrences word board =
    let
        directions =
            [ ( 1, 0 )
            , ( 0, 1 )
            , ( 1, 1 )
            , ( -1, 0 )
            , ( 0, -1 )
            , ( -1, -1 )
            , ( -1, 1 )
            , ( 1, -1 )
            ]

        wordChars : List Char
        wordChars =
            String.toList word

        height : Int
        height =
            List.length board

        width : Int
        width =
            case board of
                [] ->
                    0

                row :: _ ->
                    List.length row

        getChar : Int -> Int -> Maybe Char
        getChar row col =
            if row < 0 || col < 0 || row >= height || col >= width then
                Nothing

            else
                board
                    |> List.drop row
                    |> List.head
                    |> Maybe.andThen (\r -> List.drop col r |> List.head)

        matchesFrom : Int -> Int -> ( Int, Int ) -> Bool
        matchesFrom row col ( dRow, dCol ) =
            let
                checkIndex idx =
                    let
                        r =
                            row + idx * dRow

                        c =
                            col + idx * dCol

                        expected =
                            wordChars |> List.drop idx |> List.head
                    in
                    expected == getChar r c
            in
            List.length wordChars
                - 1
                |> List.range 0
                |> List.all checkIndex

        countAtPos : Int -> Int -> Int
        countAtPos row col =
            directions
                |> List.filter (\dir -> matchesFrom row col dir)
                |> List.length

        allPositions : List ( Int, Int )
        allPositions =
            List.range 0 (height - 1)
                |> List.concatMap (\row -> List.range 0 (width - 1) |> List.map (\col -> ( row, col )))

        totalCount =
            List.map (\( r, c ) -> countAtPos r c) allPositions
                |> List.sum
    in
    totalCount


wordCounterTest : Test
wordCounterTest =
    describe "countWordOccurrences"
        [ test "rod" <|
            \_ ->
                let
                    board =
                        [ [ 'R', 'O', 'D' ]
                        , [ 'O', 'O', 'D' ]
                        , [ 'D', 'D', 'D' ]
                        ]

                    result =
                        [ countWordOccurrences "ROD" board
                        , countWordOccurrences "OD" board
                        ]
                in
                Expect.equal result [ 3, 9 ]
        , test "sator" <|
            \_ ->
                let
                    board =
                        [ [ 'S', 'A', 'T', 'O', 'R' ]
                        , [ 'A', 'R', 'E', 'P', 'O' ]
                        , [ 'T', 'E', 'N', 'E', 'T' ]
                        , [ 'O', 'P', 'E', 'R', 'A' ]
                        , [ 'R', 'O', 'T', 'A', 'S' ]
                        ]

                    result =
                        [ countWordOccurrences "SATOR" board
                        , countWordOccurrences "AREPO" board
                        , countWordOccurrences "TENET" board
                        , countWordOccurrences "NET" board
                        , countWordOccurrences "AREP" board
                        ]
                in
                Expect.equal result [ 4, 4, 4, 4, 4 ]
        ]


tests : Test
tests =
    describe "BoardGenerator"
        [ describe "generateWordSearch"
            [ fuzz Fuzz.int "generates valid word search" <|
                \seed ->
                    let
                        words =
                            [ ( "overcomplicated", "" ) ]

                        wordU =
                            String.toUpper <| "overcomplicated"

                        wordsDict =
                            Dict.fromList words

                        generator =
                            generateWordSearch 1 ( 1, Nothing ) ( 1, 0 ) wordsDict True 1

                        result =
                            Random.step generator (Random.initialSeed seed)
                    in
                    case result of
                        ( Err _, _ ) ->
                            Expect.fail "Expected Ok but got Err"

                        ( Ok cwb, _ ) ->
                            Expect.all
                                [ \( cw, _ ) ->
                                    let
                                        firstWord =
                                            cw |> List.head |> Maybe.map (\( _, ( w, _ ), _ ) -> w)
                                    in
                                    Expect.equal firstWord (Just wordU)
                                , \( _, b ) ->
                                    let
                                        boardAsList =
                                            b |> Array.map Array.toList |> Array.toList
                                    in
                                    Expect.atLeast 1 (countWordOccurrences wordU boardAsList)
                                ]
                                cwb
            ]
        ]
