module SelectionSort exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)

-- in this function we compare the current integer with the rest of the list to get the minimum value
-- This function will return a tuple of the minimum int and the rest of the list 
getNextMinVal : Int -> List Int -> ( Int, List Int )
getNextMinVal comparedWith lst =
    case lst of
        [] ->
            ( comparedWith, [] )  -- if the list is empty we will return a tuple of int with empty list

        h :: tail ->
            if comparedWith < h then   -- if "comparedWith" int is smaller than one of the elements of list, then we return it as minimum
                let
                    recRes =
                        getNextMinVal comparedWith tail
                in
                ( Tuple.first recRes, h :: Tuple.second recRes )

            else               -- if "comparedWith" int is greater than one of the elements of list, then we return the one of the elements as minimum
                let
                    recRes =
                        getNextMinVal h tail
                in
                ( Tuple.first recRes, comparedWith :: Tuple.second recRes )


selectionSort : List Int -> List Int
selectionSort unsorted =
    case unsorted of
        [] ->
            []

        h :: [] ->
            [ h ]

        h :: tail ->
            let
                firstSorted = -- we got the minimum int from "getNextMinVal" and add it to the sorted list and keep sorting the rest of the list
                    getNextMinVal h tail
            in
            Tuple.first firstSorted :: selectionSort (Tuple.second firstSorted)

my_results: List String
my_results =
    [
       "\n bubbleSort list [19,10,8,7,6,12] \n",
       pr <| selectionSort [19,10,8,7,6,12]

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