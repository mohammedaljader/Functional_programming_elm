module OldAssigments exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)
import MathFunction exposing (Function)
import String exposing (fromInt)
import Html exposing (a)
import Html exposing (text)
import Html.Attributes exposing (list)
import Html exposing (li)
import Html exposing (i)


isPrimeBy : Int -> Int -> Bool
isPrimeBy i n =
    if i >= n then
        True

    else if modBy i n == 0 then
        False

    else
        isPrimeBy (i + 1) n

isPrime : Int -> Bool
isPrime int =
    if int == 1 then
        False
    else if int <= 0 then
      False

    -- else if int < 4  then
    --     True

    else
        -- because from 2 is the prime starting 
        isPrimeBy 2 int


filter: Int -> List Int
filter count = 
    case count of
        100 -> []
        _ -> 
           if (isPrime count) then
             count :: filter (count + 1) 
           else
             filter (count + 1) 


reverse : List a -> List a
reverse = List.foldl (\x acc -> x :: acc) []

isPalindrome : List a -> Bool
isPalindrome list = 
      if (list == reverse list) then
        True
      else
        False

removeLastElement : List a -> List a
removeLastElement list =
      case list of
         [] ->  []
         x :: [] -> []
         x:: xs -> 
          x:: removeLastElement xs

rle : List(List a) -> List(Int , a)
rle lists = 
   case lists of
    [] -> []
    x::xs ->
        case x of
            [] -> rle xs
            y :: _ -> 
                ((length x) , y) :: rle xs

removeElementN : a -> List a -> List a
removeElementN n list =
      case list of
         [] ->  []
         x:: xs -> 
           if (n /= x) then
              x :: removeElementN n xs
           else
              removeElementN n xs
           

length : List a -> Int
length list =
    case list of
        [] -> 0
        (_::xs) -> 1 + length xs


split : Int -> List a -> (List a, List a)
split splitPoint inputList =
    splitHelper splitPoint inputList []

{- We use a typical trick here, where we define a helper function 
that requires some additional arguments. -}
splitHelper : Int -> List a -> List a -> (List a, List a)
splitHelper splitPoint inputList leftSplitList =
     case inputList of
         [] ->
             -- This is a base case, we end here if we ran out of elements
             (reverse leftSplitList, [])

         head :: tail ->
              if splitPoint > 0 then
                    -- This is the recursive case
                    -- Note the typical trick here: we are shuffling elements
                    -- from the input list and putting them onto the
                    -- leftSplitList.
                    -- This will reverse the list, so we need to reverse it back
                    -- in the base cases
                    splitHelper (splitPoint - 1) tail (head :: leftSplitList)
               else
                    -- here we got to the split point,
                    -- so the rest of the list is the output
                    (reverse leftSplitList, inputList)


list1 : List number
list1 = [2,2,4,6,73,6,4,2,5]
my_results: List String
my_results =
    [
       "\nIs prime \n",
       pr <| isPrime 4,
        "\nPrime Numbers till 100 \n",
       pr <| filter 0,
       "\nSplit  \n",
       pr <| split (length list1 // 2) list1,
       "\n isPalindrome  \n",
       pr <| isPalindrome [1,2,5,2,1],
        "\n isPalindrome [2] \n", 
       pr <| isPalindrome [2],
       "\n isPalindrome [ a, b] \n", 
       pr <| isPalindrome ["a", "b"],
       "\n removeLast element [1,2,5,2,1] \n", 
       pr <| removeLastElement [1,2,5,2,1],
       "\n remove element N [1,2,5,2,1] \n", 
       pr <| removeElementN 2 [1,2,5,2,1],
       "\n RLE \n", 
       pr <| rle [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ],
        "\n RLE Strings \n", 
       pr <| rle [ [], [ "a", "a" ] ] 

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