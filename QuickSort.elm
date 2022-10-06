module QuickSort exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)

quickSort : List comparable -> List comparable
quickSort l = 
    case l of
        [] -> []
        [a] -> [a]
        a :: r ->  
            let
                left = List.filter ((>) a) r
                    |> quickSort 
                right = List.filter ((<=) a) r
                    |> quickSort 
            in
                left ++ a :: right

my_results: List String
my_results =
    [
       "\n quickSort list [19,10,8,7,6,12] \n",
       pr <| quickSort [19,10,8,7,6,12]

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