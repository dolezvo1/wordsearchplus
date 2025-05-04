
-- File licensed under MIT

module BoardGenerator exposing (WordType(..), generateWordSearch)

import Array exposing (Array)
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List
import List.Extra

type WordType = Word | Description | None

stringAt : Int -> String -> Maybe Char
stringAt idx s = s |> String.slice idx (idx+1) |> String.uncons |> Maybe.map Tuple.first

-- TODO: This is really slow. Storing the wordlist in an Array might help?
generateWordSearch : Int -> (Int, Maybe Int) -> (Float, Float) -> Dict String String -> Bool -> Float -> Generator (Result String (List (WordType, (String, String), ((Int, Int), (Int, Int))), Array (Array Char)))
generateWordSearch count (minSize, mMaxSize) (wordProb, descProb) dict allDirections maxDensity =
    let
        wordTransform w = w |> String.toUpper |> String.filter (\c -> not (List.member c [' ', '\'', '-', '(', ')', '.', '!']))
        sampleWords allWordsRandom typeSamples =
            allWordsRandom |> List.map (\w -> (wordTransform w, (w |> String.toUpper, dict |> Dict.get w |> Maybe.withDefault "")))
                           |> List.filter (\(nw, _) -> String.length nw >= minSize)
                           |> (mMaxSize |> Maybe.map (\max -> List.filter (\(nw, _) -> String.length nw <= max)) |> Maybe.withDefault (\e -> e))
                           |> List.map2 (\(ws, ds) (nw, (ow, d)) -> (nw, (if ws < wordProb then Word else if ds < descProb then Description else None, ow, d))) typeSamples
                           |> List.take count
        generateWordSearchAux (entropy, (typeSamples, allWordsRandom)) =
            let words = sampleWords allWordsRandom typeSamples
            in createWordSearch words allDirections maxDensity entropy
                |> Result.map (\(grid, ws, _) -> (ws |> List.map (\(_, (wt, ow, d), p) -> (wt, (ow, d), p)),
                                           grid |> List.map Array.fromList |> Array.fromList))
    in
        dict |> Dict.keys |> Random.List.shuffle
            |> Random.pair (Random.list count (Random.pair (Random.float 0 1) (Random.float 0 1)))
            |> Random.pair (Random.list (count * count * 250) (Random.int Random.minInt Random.maxInt))
            |> Random.map generateWordSearchAux



-- The following code was originally written by Charles Powell (in Haskell)
-- https://github.com/cekpowell/wordsearch-generator-solver/blob/main/Generator/src/Generator.hs


-----------------------
-- DATA DEFINITIONS  --
-----------------------


type alias WordSearchGrid = List (List Char)
type alias Placement = (Posn, Orientation)
type alias Posn = (Int, Int)
type Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack

dirVec : Orientation -> Posn
dirVec o = case o of
    Forward     -> (1,0)
    Back        -> (-1,0)
    Up          -> (0,-1)
    Down        -> (0,1)
    UpForward   -> (1,-1)
    UpBack      -> (-1,-1)
    DownForward -> (1,1)
    DownBack    -> (-1,1)

------------------------
-- START OF GENERATOR --
------------------------


--------------------
-- MAIN FUNCTIONS --
--------------------

{- createWordSearch
    @brief: Main function for the challenge. Given a list of words and
            a maximum density, returns a word search grid containing theese
            words in a random position and orientation, with a density less
            than the given maximum density.

            Main logic:
                - Use the list of words and maximum density to find the smallest
                  needed grid size.
                - Create an empty grid of this dimension, with blank ('~')
                  characters in every position.
                - Recurse through the words and for each word:
                    - Generate a random position.
                    - Generate a random orientation.
                    - See if the word can be placed in the current grid with this
                      placement.
                    - If it can, place it in the grid, and move onto the next word.
                    - If it cant, make another random placement and try to place again.
                    - If a word has a large number of failed attempts, assume it can't be
                      placed in the grid. Thus, a larger grid is needed for the words.
                    - Make a larger blank grid, and start from the first word again.
                - When all words are added, fill in the remainder of the blank symbols
                  with random characters from the input words.
                - Return the completed grid.
    @param: [String] : The words to be inserted into the wordsearch.
            Float : The max density of the grid to be made.
    @return: IO WordSearchGrid : The resulting grid made in the process.
             Error : If density is equal to 1 or 0 (such grids are not possible)
-}
createWordSearch : List (String, a) -> Bool -> Float -> List Int -> Result String (WordSearchGrid, List (String, a, ((Int, Int), (Int, Int))), List Int)
createWordSearch words allDirections maxDensity entropy =
    let
        newEmptyGrid = getEmptyWordSearchGrid (getMinimumGridSize (words |> List.map Tuple.first) maxDensity)
        uWords       = words |> List.map (\(w, data) -> (String.toUpper w, data))
    in if (maxDensity <= 0) || (maxDensity > 1) then Err "Invalid max density."
        else if words == [] then Ok ([], [], entropy)
            else createWordSearchAux allDirections uWords [] newEmptyGrid uWords entropy


{- createWordSearchAux
    @brief: Auxilliary function to create a word search. Given a list of words
            and an empty word search grid, recurses through the list of words,
            finding a placement for each word and then adding it to the grid.
            If a word cant be placed, the function resets with all of the words
            and a bigger grid.
    @param: [String]: The set of words to be added to the grid.
            WordSearchGrid: The empty word search grid of the needed dimension.
            [String]: The set of words to be added (not reduced under recursion
            so that the function can be reset).
    @return: IO WordSearchGrid: The generated grid containing the given words.
             Error: If one of the words is the empty string (""). Such a word
             cannot be added to a grid.
-}
createWordSearchAux : Bool -> List (String, a) -> List (String, a, ((Int, Int), (Int, Int))) -> WordSearchGrid -> List (String, a) -> List Int -> Result String (WordSearchGrid, List (String, a, ((Int, Int), (Int, Int))), List Int)
createWordSearchAux allDirections words placedWordsAcc grid allWords entropy =
    case words of
        [] -> let uniqueCharacters = allWords |> List.concatMap (Tuple.first >> String.toList)
              in fillBlanks uniqueCharacters grid entropy |> Result.map (\(g, e) -> (g, placedWordsAcc, e))
        ((w,data)::restOfWords) -> if w == "" then Err "Invalid input word: Words length must be >= 1." else -- cant insert empty word into a grid
            case getRandomPlacement allDirections w grid 0 entropy of
                Err e -> Err e
                Ok (((sx,sy) as sp, dir), entropy2) ->
                    if (sp, dir) == ((0, 0), Up) then  -- word cant be placed - bigger grid needed
                        let newBiggerDimension = floor ((List.length grid |> toFloat) * 1.5)
                            newBiggerGrid = (getEmptyWordSearchGrid newBiggerDimension)
                        in createWordSearchAux allDirections allWords [] newBiggerGrid allWords entropy2
                    else
                        let updatedGrid = addWord w (sp, dir) grid
                            (dx, dy) = dirVec dir
                            remainingLen = (String.length w) - 1
                            newWord = (w, data, (sp, (sx + dx * remainingLen, sy + dy * remainingLen)))
                        in createWordSearchAux allDirections restOfWords (newWord::placedWordsAcc) updatedGrid allWords entropy2

--------------------------------------
-- GETTING NEEDED DIMENSION OF GRID --
--------------------------------------

{- getMinimumGridSize
    @brief: Given the set of words to be added and the max density of the grid,
            the function will calculuate the minimum dimension of the grid.
            The dimension of the grid must be large enough that the ration between
            the characters in the words and the filler characters is less than the
            max density. Also, the grid must be large enough to fit the largest word in
            the set into it. Thus, the minimum grid dimension will be the max of the
            length of the largest word or the dimension according to the density.
    @param:[String]: words to be added to the grid.
           Float: Max density of the grid.
    @return: Int: The minimum dimension of a grid to satisfy the constraints.
-}
getMinimumGridSize : List String -> Float -> Int
getMinimumGridSize words maxDensity =
    let gridSizeByDensity    = getMinimumGridSizeByDensity words maxDensity
        gridSizeByWordLength = getMinimumGridSizeByWordLength words
    in max (gridSizeByDensity) (gridSizeByWordLength)

{- getMinimumGridSizeByDensity
    @brief: Given a a set of words, and a max density, the function will calculute the
            dimension of the smallest grid that can will have a density less than the max
            density. The minimum grid size found by density is an nxn grid where nxn is
            the next square number after (number of characters in words)/maxDensity.
            A grid of this size will always have a density less than the given maximum
            density.
    @param: [String]: Set of words to be added.
            Float: Max density of grid.
    @return: Int: Minimum dimension of the grid to ensure density is less than max density.
-}
getMinimumGridSizeByDensity : List String -> Float -> Int
getMinimumGridSizeByDensity words maxDensity =
    let totalCharacters    = words |> List.map String.length |> List.sum
        minimumCellsNeeded = ceiling ((toFloat totalCharacters) / maxDensity)
    in nextSquareNumber minimumCellsNeeded 0

{- nextSquareNumber
    @brief: Helper function to calculate the minimum dimension according to density.
            Recursivley finds the root of the next square number after the given number.
    @param: Int: The number for which we want to find the next square number.
            Int: The current guess for the root of the next square number.
    @return: Int: The root of the next square number after the input number.
-}
nextSquareNumber : Int -> Int -> Int
nextSquareNumber number currentGuess =
    if (currentGuess*currentGuess) > number then currentGuess
    else nextSquareNumber number (currentGuess+1)

{- getMinimumGridSizeByWordLength
    @brief: Given a set of words, calculates minimum dimension for a grid to contain
            all of these words. The minimum grid size found by length is a grid nxn
            where n is the length of the longest word in the given list. Thus function
            finds the longest word by recursivley going through each word and seeing if
            its length is greater than the current length.
    @param: [String]: The set of words.
    @return: Int: The length of the largest word.
-}
getMinimumGridSizeByWordLength : List String -> Int
getMinimumGridSizeByWordLength words = getMinimumGridSizeByWordLengthAux words 0

{- getMinimumGridSizeByWordLengthAux
    @brief: Auxilliary function for 'getMinimumGridSizeByWordLength'. Recursivley
            finds the length of the largest word in the list of words.
    @param: [String]: The list of words.
            Int: Length of the current largest word.
    @return: Int: Length of the largest word in the list.
-}
getMinimumGridSizeByWordLengthAux : List String -> Int -> Int
getMinimumGridSizeByWordLengthAux words currentMaxWordLength =
    case words of
        [] -> currentMaxWordLength
        (w::remainingWords) -> getMinimumGridSizeByWordLengthAux remainingWords (max (String.length w) (currentMaxWordLength))

-------------------------
-- CREATING EMPTY GRID --
-------------------------

{- getEmptyWordSearchGrid
    @brief: Given a dimension ,n, creates an empty n x n grid with this dimension.
            This is a list of n items, each of which contains n copies of '~'. These
            are a special character used to signify that no letter has been placed at
            this position in the grid yet (used when finding a placement for a word.)
    @param: Int: The dimension of the grid.
    @return: WordSearchGrid: A grid of the specified dimension, with every position
             set to '~'.
-}
getEmptyWordSearchGrid : Int -> WordSearchGrid
getEmptyWordSearchGrid n = '~' |> List.repeat n |> List.repeat n

-----------------------------------------
-- FINDING RANDOM PLACEMENT FOR A WORD --
-----------------------------------------

{- getRandomPlacement
    @brief: Given a word, a word search grid, and a counter, attempts
            to find a random placement for the word within the grid.
            Main logic:
                - Use helper functions to generate a random position and random
                  orientation for the word.
                - Use helper functions to check if this placement of the word in
                  the grid is valid.
                - If it is, return the placement.
                - If it is not, recall the function so that another random placement
                  is tried.
                - A counter records number of times the function has attempted
                  to find a placement for this word. If the counter is much larger
                  than (n x n), the chances are that the word cannot be placed.
                - In this case, return an INVALID placement to signify the word
                  cannot be palced in the grid, and the calling function will create a
                  new grid.
    @param: String: The word for which a placement is being seeked.
            WordSearchGrid: The grid we are trying to place the word into.
    @return: A placement if one was found. If no placement was found, returns
             ((0,0), Nothing), which is an invalid placement.
-}
getRandomPlacement : Bool -> String -> WordSearchGrid -> Int -> List Int -> Result String (Placement, List Int)
getRandomPlacement allDirections word grid attempts entropy =
    if (attempts >= (List.length grid) * (List.length grid) * 4) then Ok (((0,0),Up), entropy)
    else case getRandomOrientation allDirections entropy of
        Err e -> Err e
        Ok (orientation, entropy2) ->
            case getRandomPosition (List.length grid) entropy2 of
                Err e -> Err e
                Ok (position, entropy3) ->
                    let
                        placement = (position, orientation)
                        correct = isCorrectPlacement word placement grid
                    in if correct then
                        Ok (placement, entropy3)
                    else
                        (getRandomPlacement allDirections word grid (attempts+1) entropy3)

{- getRandomPosition
    @brief: Given the dimension of a grid, returns a random position within
            the grid.
    @param: Int: Dimension of the grid.
    @return: IO Posn: The randomly generated position within the grid.
-}
getRandomPosition : Int -> List Int -> Result String (Posn, List Int)
getRandomPosition dimension entropy =
    case entropy of
        (c::r::remainingEntropy) -> Ok ((modBy dimension c, modBy dimension r), remainingEntropy)
        _ -> Err "Insufficient entropy"

{- getRandomOrientation
    @brief: Returns a random orientation by generating a random number and indexing
            the list of all orientations.
    @param: N/A
    @return: IO Orientation: A randomly generated orientation
-}
getRandomOrientation : Bool -> List Int -> Result String (Orientation, List Int)
getRandomOrientation allDirections entropy =
    let orientations = [Forward, Down, DownForward, Back, Up, UpForward, UpBack, DownBack]
    in case entropy of
        [] -> Err "Insufficient entropy"
        (e::remainingEntropy) -> Ok (orientations |> List.Extra.getAt (modBy (if allDirections then 8 else 3) e) |> Maybe.withDefault Forward, remainingEntropy)

{- isCorrectPlacement
    @brief: Given a word, placement and grid, will verify if the word can be placed in the
            grid at this position. This is done by checking pattern matching the orientation
            of the word, and using a helper function to check if the word can be placed in
            the given direction at the given position. The orientation is passed in as a vector
            representing one unit of movement in the given orientation.
    @param: String: Word being verified.
            Placement: The placement being verified.
            WordSearchGrid: The grid the word is to be placed in.
    @return: Bool: True if the word can be placed, False if not.
-}
isCorrectPlacement : String -> Placement -> WordSearchGrid -> Bool
isCorrectPlacement word ((c,r), orientation) grid =
    isCorrectPlacementAux word (c,r) (dirVec orientation) grid

{- isCorrectPlacementAux
    @brief: Given a word, position, orientation vector and grid, determines if the word
            can be placed at the given position, with the given direction. Does this by
            recursing through the letters of the word, and breaking if at any point there
            is a block in the path, or if the word goes beyong the limits of the grid.
            A letter can be placed at the current position if the current position is
            the same letter as it, or if it is a '~', signifying that a letter has not
            yet been placed at this position.
    @param: String: Word being verified.
            Posn: Position the word is being placed at.
            (Int, Int): Directiion vector for the orientation of the word.
            WordSearchGrid: The grid the word is to be placed in.
    @return: Bool: True if the placement is valid, False if not.
-}
isCorrectPlacementAux : String -> Posn -> (Int, Int) -> WordSearchGrid -> Bool
isCorrectPlacementAux word ((cc,cr) as currentPosition) ((dc,dr) as direction) grid =
    case String.uncons word of
        Nothing -> True
        Just (l, restOfWord) -> if isOutOfBoundsC2 currentPosition grid then False
            else case grid |> List.Extra.getAt cr |> Maybe.andThen (List.Extra.getAt cc) of
                Nothing -> False
                Just ol -> if (ol /= '~') && (ol /= l) then False -- this cell isnt free, and isnt a  match
                    else isCorrectPlacementAux restOfWord (cc+dc, cr+dr) direction grid-- this cell is free or is a  match

{-outOfBoundsC2
    @brief: Given a position and a grid, determines if this position is
            out of bounds of the grid.
    @param: (Int,Int): The postion being checked.
    @return: True if the given position is out of bounds, False if not.
-}
isOutOfBoundsC2 : (Int, Int) -> WordSearchGrid -> Bool
isOutOfBoundsC2 ((c,r) as position) grid = (c >= List.length grid) || (r >= List.length grid) || (c < 0) || (r < 0)

-------------------
-- ADDING A WORD --
-------------------

{- addWord
    @brief: Given a word, a placement and a grid, will place the word into the grid.
            This is done by pattern matching the orientation of the placement, and
            calling a helper function to place the word into the grid in the given
            orientation. The orientation is passed in as a vector representing a unit
            of movement in the direction of the  orientation.
    @param: String: Word being placed in the grid.
            Placement: The placement of the word.
            WordSearchGrid: The grid the word is being inserted into.
    @return: WordSearchGrid: Returns the input wordsearch grid, with the word added.
-}
addWord : String -> Placement -> WordSearchGrid -> WordSearchGrid
addWord word (((c,r), orientation) as placement) grid =
    addWordAux word (c,r) (dirVec orientation) grid


{- addWordAux
    @brief: Given a word, position of placement, direction vector and grid, will place
            the word into the grid at this position in the orientation dictated by the
            direction vector. This is done by recursing through the letters in the word,
            and calling a helper function to add each letter of the word individually.
    @param: String: The word being added to the grid.
            Posn: The position where the word must be added.
            (Int,Int): The direction vector representing the orientation of the word.
            WordSearchGrid: The grid the word is being inserted into.
    @return: WordSearchGrid: The input grid with the given word inserted.
-}
addWordAux : String -> Posn -> (Int, Int) -> WordSearchGrid -> WordSearchGrid
addWordAux word ((cc,cr) as currentPosition) ((dc,dr) as direction) grid =
    case String.uncons word of
        Nothing -> grid
        Just (l, word2) -> addWordAux word2 (cc+dc,cr+dr) direction (addLetter l grid currentPosition)

{- addLetter
    @brief: Given a letter, grid, and position, will add the letter into this positiion
            of the grid. Does this by recursing through the rows of the grid until in the
            needed row, and then calling a helper function to add the letter into the
            needed column of this row.
    @param: Char: The letter being added.
            WordSearchGrid: The grid the letter is being added into.
            Posn: The position the letter is to be insserted into.
    @return: WordSearchGrid: The input grid with the letter inserted at the given position.
-}
addLetter : Char -> WordSearchGrid -> Posn -> WordSearchGrid
addLetter l grid ((c,r) as position) =
    case grid of
        [] -> []
        (row::restOfGrid) ->
            if r == 0 then (addLetterAux l row c)::restOfGrid
            else row::(addLetter l restOfGrid (c,r-1))

{- addLetterAux
    @brief: Given a letter, a row within a grid, and a column number, will insert the
            letter into this column of the row. Does this by recursing through the columns
            in the row until it finds the needed column.
    @param: Char: The letter being inserted into the grid.
            [Char]: The row the letter is to be inserted into
            Int: The column number the letter is to be inserted at.
    @return: [Char]: The input row with the input letter at the input column number.
-}
addLetterAux : Char -> List Char -> Int -> List Char
addLetterAux l row c =
    case row of
        [] -> []
        (x::xs) -> if c == 0 then l::xs else x::(addLetterAux l xs (c-1))

---------------------------
-- FILLING IN THE BLANKS --
---------------------------

{- fillBlanks
    @brief: Given the set of characters in all words and the wordsearch grid, will
            replace all of the blank ('~) characters within the grid with a random
            character from the set of characters. This is done by recursing through
            the grid and calling a helper function on each row within the grid.
    @param: [Char]: The set of all characters within the input words.
            WordSearchGrid: The grid for which the blanks need to be filled.
    @return: IO WordSearchGrid: The input grid with all of the blanks replaced with
             random characters.
-}
fillBlanks : List Char -> WordSearchGrid -> List Int -> Result String (WordSearchGrid, List Int)
fillBlanks characters grid entropy =
    case grid of
        [] -> Ok ([], entropy)
        (row::restOfGrid) ->
            case fillBlanks characters restOfGrid entropy of
                Err e -> Err e
                Ok (remainder, entropy2) ->
                    case fillBlanksAux characters row entropy2 of
                        Err e -> Err e
                        Ok (newRow, entropy3) -> Ok (newRow::remainder, entropy3)

{- fillBlanksAux
    @brief: Given the set of characters, and a row from a grid, will replace
            all '~' within the grid with a random character, taken from the
            list of input characteres. This is done by recursing through the
            row and checking if the character at this position is a '~' or not.
    @param: [Char]: The set of characters within the input words.
            [Char]: The row for which the blanks must be replaced.
    @return: IO [Char]: The input row with all of the '~' replaced with a random
             character from the input list.
-}
fillBlanksAux : List Char -> List Char -> List Int -> Result String (List Char, List Int)
fillBlanksAux characters row entropy =
    case row of
        [] -> Ok ([], entropy)
        (l::restOfRow) ->
            case fillBlanksAux characters restOfRow entropy of
                Err e -> Err e
                Ok (remainder, entropy2) ->
                    case (if (l == '~') then getRandomChar characters entropy2 else Ok (l, entropy2)) of
                        Err e -> Err e
                        Ok (newL, entropy3) -> Ok (newL::remainder, entropy3)

{- getRandomChar
    @brief: Given a list of characters, returns a random character from this list.
    @param: [Char]: The list of characters.
    @return: IO Char: A random character from the input list.
-}
getRandomChar : List Char -> List Int -> Result String (Char, List Int)
getRandomChar characters entropy =
    case entropy of
        [] -> Err "Insufficient entropy"
        (e::remainingEntropy) -> Ok (characters |> List.Extra.getAt (modBy (List.length characters) e) |> Maybe.withDefault '~', remainingEntropy)


----------------------
-- END OF GENERATOR --
----------------------
