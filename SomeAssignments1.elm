module SomeAssignments1 exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)
import String exposing (contains, foldl, fromChar, toUpper)

-- Sum of Multiples --
sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    List.sum (List.filter (inMultiples divisors) (List.range 1 (limit - 1)))


inMultiples : List Int -> Int -> Bool
inMultiples divisors candidate =
    List.any (\divisor -> modBy divisor candidate == 0) divisors

-- End sum of Multiples --


-- SubList --
type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist =
    if alist == blist then
        Equal

    else if inList alist blist then
        Superlist

    else if inList blist alist then
        Sublist

    else
        Unequal


inList : List a -> List a -> Bool
inList alist blist =
    let
        getLastInList sublist_ =
            Maybe.withDefault [] (List.tail sublist_)
    in
    if List.length alist < List.length blist then
        False

    else if List.take (List.length blist) alist == blist then
        True

    else
        inList (getLastInList alist) blist

-- End SubList --

-- Strain --
keep : (a -> Bool) -> List a -> List a
keep predicate list =
    List.foldr (consIf predicate) [] list


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    List.foldr (consIf (\v -> not (predicate v))) [] list


consIf : (a -> Bool) -> a -> List a -> List a
consIf predicate value list =
    if predicate value then
        value :: list

    else
        list
        
even : Int -> Bool
even number =
    modBy 2 number == 0


odd : Int -> Bool
odd number =
    modBy 2 number == 1


isFirstLetter : String -> String -> Bool
isFirstLetter letter word =
     String.left 1 word == letter


lessThanTen : Int -> Bool
lessThanTen num =
    num < 10

-- End Strain --

-- Series --
slices : Int -> String -> Result String (List (List Int))
slices size input =
    if size < 0 then
        Err "slice length cannot be negative"

    else if size == 0 then
        Err "slice length cannot be zero"

    else if  String.isEmpty input then
        Err "series cannot be empty"

    else if  String.length input < size then
        Err "slice length cannot be greater than series length"

    else
         String.split "" input
            |> List.map  String.toInt
            |> combine
            |> Maybe.map (takeRuns size)
            |> Result.fromMaybe ""


takeRuns : Int -> List Int -> List (List Int)
takeRuns size numbers =
    let
        candidate =
            List.take size numbers
    in
    if List.length candidate < size || size < 1 then
        []

    else
        candidate :: takeRuns size (List.drop 1 numbers)


combine : List (Maybe a) -> Maybe (List a)
combine =
    List.foldr (Maybe.map2 (::)) (Just [])

-- End Series --

--- ScrabbleScore  --- 
addLetterScore : Char -> Int -> Int
addLetterScore s total =
    let
        c =
            toUpper (fromChar s)
    in
    if contains c "AEIOULNRST" then
        total + 1

    else if contains c "DG" then
        total + 2

    else if contains c "BCMP" then
        total + 3

    else if contains c "FHVWY" then
        total + 4

    else if contains c "K" then
        total + 5

    else if contains c "JX" then
        total + 8

    else if contains c "QZ" then
        total + 10

    else
        total


scoreWord : String -> Int
scoreWord x =
    foldl addLetterScore 0 x

--- End ScrabbleScore ---- 

my_results: List String
my_results =
    [
         "\n ---(sumOfMultiples)---- \n",
       "\n sumOfMultiples [1,2] 18 \n",
       pr <| sumOfMultiples [1, 2, 3] 4,
        "\n ----(SubLists)----- \n",
       "\n SubList \n",
       pr <| sublist [1, 2, 3] [1,2],
        "\n InList \n",
       pr <| inList [1, 2, 3] [1, 2],
        "\n ---- Strain ---- \n",
        "\n keep lessThanTen [ 1, 2, 3 ] \n",
        pr <| keep lessThanTen [ 1, 2, 3 ],
        "\n keep odd [ 1, 2, 3 ] \n",
        pr <| keep odd [ 1, 2, 3 ],
        "\n keep (isFirstLetter z) [ apple, zebra, banana, zombies, cherimoya, zealot ] \n",
        pr <| keep (isFirstLetter "z") [ "apple", "zebra", "banana", "zombies", "cherimoya", "zealot" ],
         "\n discard odd [ 1, 2, 3 ] \n",
        pr <| discard odd [ 1, 2, 3 ],
        "\n discard even [ 1, 3, 2,5, 7 ] \n",
        pr <| discard even [ 1, 3,2, 5, 7 ],
         "\n discard (isFirstLetter z) [ apple, zebra, banana, zombies, cherimoya, zealot ] \n",
        pr <| discard (isFirstLetter "z") [ "apple", "zebra", "banana", "zombies", "cherimoya", "zealot" ],
         "\n ---- Series ---- \n",
         "\nslices 3 777777 \n",
        pr <| slices 3 "777777" ,
          "\nslices 2 777777 \n",
        pr <| slices 2 "777777",
         "\n ---- ScrabbleScore ---- \n",
        "\n scoreWord f \n",
        pr <| scoreWord "f"

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