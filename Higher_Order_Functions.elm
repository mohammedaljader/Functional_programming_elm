module Higher_Order_Functions exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)
import MathFunction exposing (Function)
import Debug exposing (toString)
import Html.Attributes exposing (list)


findKey : k -> List (k,v) -> Maybe v
findKey key = List.foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

applyTwice : (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f list1 list2 = 
    case (list1, list2) of
        (_, []) -> []
        ([], _) -> []
        (x::xs, y::ys) -> f x y :: zipWith f xs ys


flip : (a -> b -> c) -> b -> a -> c
flip f y x = f x y

map : (a -> b) -> List a -> List b
map f list = 
  case list of
    [] -> []
    (x::xs) -> f x :: map f xs

filter : (a -> Bool) -> List a -> List a
filter p list = 
  case list of
    [] -> []

    (x::xs) ->
        if p x then
            x :: filter p xs
        else
            filter p xs

takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list = case list of
    [] -> []
    (x::xs) -> 
        if p x then
        x :: takeWhile p xs 
        else []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile p list = case list of
    [] -> []
    (x::xs) -> 
      if p x then 
        dropWhile p xs 
      else list

length : List a -> Int
length list =
    case list of
        [] -> 0
        (_::xs) -> 1 + length xs



chain : Int -> List Int
chain n = 
    case n of
        1 -> [1]
        
        _ ->
            if (modBy 2 n == 0) then 
                n :: chain (n // 2)
            else 
                n :: chain (n * 3 + 1)

sum : List number -> number
sum xs = 
   List.foldl (\x acc -> acc + x) 0 xs

map1 : (a -> b) -> List a -> List b
map1 f xs = 
    List.foldr (\x acc -> f x :: acc) [] xs

reverse : List a -> List a
reverse = List.foldl (\x acc -> x :: acc) []



product : List Int -> Int
product list = 
    case list of
     [] ->
       1
     x :: xs ->
       x * product xs


-- sqrtSums : Int
-- sqrtSums =
--     let
--         sqrts = List.map sqrt (List.map toFloat (List.range 1 1000))
--     in
--     List.length (takeWhile (flip (<) 1000) (List.scanl (+) 1 sqrts))

-- oddSquareSum : Int
-- oddSquareSum = 
--     let
--         odd n = n % 2 == 1
--     in
--     List.sum (takeWhile (flip (<) 10000)
--         (List.filter odd (List.map (flip (^) 2) (List.range 1 9999))))

-- oddSquareSum : Int
-- oddSquareSum =
--     let
--         odd n = n % 2 == 1
--         oddSquares = List.filter odd <| List.map (flip (^) 2) (List.range 1 9999)
--         belowLimit = takeWhile (flip (<) 10000) oddSquares
--     in
--         List.sum belowLimit

filter1 : (a -> Bool) -> List a -> List a
filter1 p = 
   List.foldr (\x acc -> if p x then x :: acc else acc) []

-- Contains function
contains : a -> List a -> Bool
contains y ys = 
    List.foldl (\x acc -> if x == y then True else acc) False ys

contains1 : a -> List a -> Bool
contains1 value list =
    case list of
        [] -> False
        (x::xs) ->
            if x == value then
                True
            else 
                contains1 value xs

-- maximum : List comparable -> comparable
-- maximum list =
--     case list of
--         [] -> Debug.crash "maximum of empty list"
--         [x] -> x
--         (x::xs) ->
--             let 
--                 maxTail = maximum xs
--             in
--                 if x > maxTail then
--                     x
--                 else
--                     maxTail

maximum : List Int -> Int
maximum list =
    case list of
        [] -> 0
        [x] -> x
        (x::xs) ->
            let 
                maxTail = maximum xs
            in
                if x > maxTail then
                    x
                else
                    maxTail

repeat : Int -> a -> List a
repeat n x =
    if n <= 0 then
        []
    else 
        x :: repeat (n - 1) x

take : Int -> List a -> List a
take n list =
    if n <= 0 then
        []
    else
        case list of
            [] -> []
            (x::xs) -> x :: take (n - 1) xs


reverse1 : List a -> List a
reverse1 list =
    case list of
        [] -> []
        (x::xs) -> reverse1 xs ++ [x]

zip : List a -> List b -> List (a, b)
zip list1 list2 =
    case (list1, list2) of
        (_, []) -> []
        ([], _) -> []
        ((x::xs), (y::ys)) -> (x, y) :: zip xs ys

quicksort : List comparable -> List comparable
quicksort list = 
    case list of
        [] -> []
        (x::xs) ->
            let 
                smallerSorted = quicksort (List.filter ((>) x) xs)
                biggerSorted = quicksort (List.filter ((<=) x) xs)
            in  
                smallerSorted ++ [x] ++ biggerSorted

my_results: List String
my_results =
    [
       "\n applyTwice \n",
        pr <| applyTwice ((+) 7) 8,
        "\n ZipWith \n",
        pr <| zipWith (+) [4,2,5,6] [2,6,2,3],
        "\n ZipWith Max \n",
        pr <| zipWith max [4,2,5,6] [2,6,2,3],
        "\n ZipWith String \n",
        pr <| zipWith (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"],
        "\n ZipWith repeat \n",
        pr <| zipWith (*) (List.repeat 5 2) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         "\n Flip  \n",
        pr <| zipWith (flip (/)) (List.repeat 5 2) [10,8,6,4,2],
         "\n Map   \n",
        pr <| map ((+) 3) [1,5,3,1,6],
         "\n Map   \n",
        pr <| map (flip (++) "!") ["BIFF", "BANG", "POW"],
         "\n Map   \n",
        pr <| map (List.repeat 3) [3, 4, 5, 6],
        "\n Map   \n",
        pr <| map (List.map (flip (^) 2)) [[1,2],[3,4,5,6],[7,8]],
        "\n Map   \n",
        pr <| map Tuple.first [(1,2),(3,5),(6,3),(2,6),(2,5)],
        "\n Filter   \n",
        pr <| filter ((<) 3) [1,5,3,2,1,6,4,3,2,1],
        "\n Filter   \n",
        pr <| filter ((==) 3) [1,2,3,4,5],
        "\n takeWhile   \n",
        pr <| takeWhile ((>) 6) [1,5,3,2,1,6,4,3,2,1],
        "\n Chain   \n",
        pr <| chain 10,
         "\n Sum using foldl   \n",
        pr <| sum [1,2,3], 
        "\n Product   \n",
        pr <| product [1,2,3],
        "\n DropWhile   \n",
        pr <| dropWhile ((>) 8) [1,2,3, 8, 9, 10],
         "\n Max   \n",
        pr <| maximum [1,2,3, 8, 9, 10],
        "\n Repeat   \n",
        pr <| repeat 2 "hello",
         "\n take   \n",
        pr <| take 2 ["hello","hello","hello","hello"],
        "\n Reverse   \n",
        pr <| reverse1  ["hello","hell","hel","he" , "h"],
        "\n Zip   \n",
        pr <| zip  ["hello","hell","hel","he" , "h"]  ["w", "w"],
         "\n quicksort   \n",
        pr <| quicksort  [3,1,2,5,4,5,9]
        --  "\n FindKey   \n",
        -- pr <| findKey "penny" phoneBook
        


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