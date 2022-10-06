module SomeAssignments3 exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)
import String exposing (length)
import String exposing (toList)
import Array exposing (Array)


--- Heap Year ----
isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && (not (modBy 100 year == 0) || modBy 400 year == 0)
--- End Heap Yeap ---- 

--- largest product of 2 numbers ---- 
largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    if length < 0 then
        Nothing

    else if length == 0 then
        Just 1

    else
         String.split "" series
            |> List.map  String.toInt
            |> combineResults
            |> Maybe.map
                (takeRuns length
                    >> List.map List.product
                    >> List.maximum
                )
            |> joinMaybe


takeRuns : Int -> List a -> List (List a)
takeRuns size items =
    let
        candidate =
            List.take size items
    in
    if List.length candidate < size || size < 1 then
        []

    else
        candidate :: takeRuns size (List.drop 1 items)



-- inlined from Results.Extra and Maybe.Extra


combineResults : List (Maybe a) -> Maybe (List a)
combineResults =
    List.foldr (Maybe.map2 (::)) (Just [])


joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe mx =
    case mx of
        Just x ->
            x
        Nothing ->
            Nothing

--- End largest product of 2 numbers ----

--- Isogram ---  
isAlpha : Char -> Bool
isAlpha char =
    Char.isUpper char || Char.isLower char


isIsogram : String -> Bool
isIsogram sentence =
    let
        sanitized =
             String.filter isAlpha sentence
                |>  String.toLower
                |>  String.toList
                |> List.sort
                |> group
    in
    List.all (\x -> List.length x == 1) sanitized



-- Adapted from https://github.com/elm-community/list-extra


group : List a -> List (List a)
group list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( ys, zs ) =
                    span ((==) x) xs
            in
            (x :: ys) :: group zs


span : (a -> Bool) -> List a -> ( List a, List a )
span p xs =
    ( takeWhile p xs, dropWhile p xs )


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileHelper acc list =
            case list of
                [] ->
                    List.reverse acc

                x :: xs ->
                    if predicate x then
                        takeWhileHelper (x :: acc) xs

                    else
                        List.reverse acc
    in
    takeWhileHelper []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list
--- End Isogram --- 
--- Distance Between two strings ----
distance : String -> String -> Result String Int
distance left right =
    if length left /= length right then
        Err "strands must be of equal length"

    else
        List.map2 (\l r -> l /= r) (toList left) (toList right)
            |> List.filter identity
            |> List.length
            |> Ok
--- EndDistance Between two strings ---

-- Grains  square --
square : Int -> Maybe Int
square n =
    if n < 1 then
        Nothing

    else
        Just <| 2 ^ (n - 1)
---- End Grains  square ---- 

---    squareOfSum     ----
squareOfSum : Int -> Int
squareOfSum n =
    let
        sum =
            n * (n + 1) // 2
    in
    sum * sum


sumOfSquares : Int -> Int
sumOfSquares n =
    List.sum (List.map (\m -> m * m) (List.range 0 n))


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n
---END squareOfSum ---

--- COllatz Steps ---
collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Err "Only positive integers are allowed"

    else
        Ok (collatzHelper 0 start)


collatzHelper : Int -> Int -> Int
collatzHelper steps start =
    if start == 1 then
        steps

    else if modBy 2 start == 0 then
        collatzHelper (1 + steps) (start // 2)

    else
        collatzHelper (1 + steps) (3 * start + 1)
---End Collatz steps ---

--- Binary Search --- 
find : Int -> Array Int -> Maybe Int
find target xs =
    find_ 0 (Array.length xs) target xs


find_ : Int -> Int -> Int -> Array Int -> Maybe Int
find_ min max target xs =
    let
        middle =
            (min + max) // 2

        middleComp =
            Array.get middle xs
                |> Maybe.map (compare target)
    in
    if max < min then
        Nothing

    else
        case middleComp of
            Just LT ->
                find_ min (middle - 1) target xs

            Just GT ->
                find_ (middle + 1) max target xs

            Just EQ ->
                Just middle

            Nothing ->
                Nothing

--- End Binary Search
my_results: List String
my_results =
    [
       "\n --- Heap Year ----  \n",  
       "\n Heap Year 1996 \n",
       pr <| isLeapYear 1996,
        "\n Heap Year 1998 \n",
       pr <| isLeapYear 1998,
       "\n --- largest product of 2 numbers ----  \n",  
       "\n largestProduct 2 0123456789 \n",
       pr <| largestProduct 2 "0123456789",
        "\n largestProduct 3 576802143 \n",
       pr <| largestProduct 3 "576802143",
        "\n --- Isogram ----  \n",  
       "\n isogram with only lower case characters \n",
       pr <| isIsogram "isogram",
        "\n word with one duplicated character \n",
       pr <| isIsogram "eleven",
       "\n --- distance  between 2 strings ----  \n",  
       "\n complete distance in single nucleotide strands A -> B\n",
       pr <| distance "A" "G",
        "\n complete distance in small strands AG -> CT \n",
       pr <| distance "AG" "CT",
        "\n --- Grains  square 2 ----  \n",  
       "\n of 2 \n",
       pr <| square 2,
        "\n of 2 \n",
       pr <| square 3 ,
       "\n --- squareOfSum  ----  \n",  
       "\n quare the sum of the numbers up to the given number squareOfSum 5 \n",
       pr <| squareOfSum 5,
        "\n square of sum 10\n",
       pr <| squareOfSum 10,
       "\n --- Collazts Steps  ----  \n",  
       "\n divide if even \n",
       pr <| collatz 16,
        "\n even and odd step \n",
       pr <| collatz 12,
        "\n --- Binary Search  ----  \n",  
       "\n finds a value in the middle of an array \n",
       pr <| find 6 (Array.fromList [ 1, 3, 4, 6, 8, 9, 11 ]),
        "\n Array.fromList [ 1, 3, 4, 6, 8, 9, 11 ] \n",
       pr <| find 1 (Array.fromList [ 1, 3, 4, 6, 8, 9, 11 ])

    ] 
    


page_width : number
page_width = 1000

to_wrap: String -> String
to_wrap my_value =
    if ( String.length my_value <= page_width) then
        ( String.left page_width my_value)
    else
        ( String.left page_width my_value) ++ ("\n") ++ to_wrap ( String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr : a -> String
pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)