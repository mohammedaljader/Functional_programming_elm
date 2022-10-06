module BubbleSort exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)

bubble : List Int -> List Int -> List Int -> List Int
bubble source tempList result =
        case source of
            [] ->
                result      

            h1 :: [] ->
                sort tempList (h1 :: result)

            h1 :: h2 :: tail ->
                if h1 < h2 then
                    bubble (h2 :: tail) (h1 :: tempList) result

                else
                    bubble (h1 :: tail) (h2 :: tempList) result
                


sort : List Int -> List Int -> List Int
sort source result =
            if List.isEmpty source then
                result

            else
                bubble source [] result

bubbleSort : List Int -> List Int
bubbleSort inputList =
    sort inputList []



--- The second Way -----
bubbleSort1 : List comparable -> List comparable
bubbleSort1 l =
    let
        bubbleSortWrapper i ll =
            let
                newList = bubbleSortAux (i - 1) ll
            in
            if newList == ll then
                newList
            else
                bubbleSortWrapper (i - 1) newList
    in
    bubbleSortWrapper (List.length l) l

bubbleSortAux : Int -> List comparable -> List comparable
bubbleSortAux i l =
    if i == 0 then
        l
    else case l of
        [] -> []
        a :: [] -> [a]
        a :: b :: r ->
            if a > b then
                b :: bubbleSortAux (i - 1) (a :: r)
            else
                a :: bubbleSortAux (i - 1) (b :: r)



my_results : List String
my_results =
    [
       "\n bubbleSort list [19,10,8,7,6,12] \n",
       pr <| bubbleSort [19,10,8,7,6,12]

    ] 
    


page_width : number
page_width = 1000

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr : a -> String
pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)