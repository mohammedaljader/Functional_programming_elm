module PatternMatching exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)
import Debug exposing (toString)

lucky : Int -> String
lucky x =
    case x of
        7 -> "LUCKY NUMBER SEVEN!"
        n -> "Sorry, you're out of luck, pal! You had " ++ toString n


sayMe : Int -> String
sayMe x = 
    case x of
        1 -> "One!"
        2 -> "Two!"
        3 -> "Three!"
        4 -> "Four!"
        5 -> "Five!"
        _ -> "Not between 1 and 5"

factorial : Int -> Int
factorial n =
    case n of
        0 -> 1
        _ -> n * factorial (n - 1)

charName : Char -> String
charName c = 
    case c of
        'a' -> "Albert"
        'b' -> "Broseph"
        'c' -> "Cecil"
        _   -> "Not this letter"

addVectors : (Float, Float) -> (Float, Float) -> (Float, Float)
addVectors a b = 
    (Tuple.first a + Tuple.first b, Tuple.second a + Tuple.second b)

addVectorsB : (Float, Float) -> (Float, Float) -> (Float, Float)
addVectorsB (x1, y1) (x2, y2) = 
    (x1 + x2, y1 + y2)

first : (a, b, c) -> a
first (x, _, _) = x

second : (a, b, c) -> b
second (_, y, _) = y

third : (a, b, c) -> c
third (_, _, z) = z

member : a -> List a -> Bool
member value list =
    case list of
        [] -> False
        (x::xs) ->
            if x == value then
                True
            else 
                member value xs

length : List a -> Int
length list =
    case list of
        [] -> 0
        (_::xs) -> 1 + length xs

sum : List Int -> Int
sum list =
    case list of
        [] -> 0
        (x::xs) -> x + sum xs

cylinder : Float -> Float -> Float
cylinder r h =
    let 
        sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  
        sideArea + 2 * topArea

tell : List a -> String
tell list =
    case list of
        [] -> "The list is empty"
        (x::[]) -> "The list has one element: " ++ toString x
        (x::y::[]) -> "The list has two elements: " ++ toString x ++ " and " ++ toString y
        (x::y::_) -> "This list is long. The first two elements are: " ++ toString x ++ " and " ++ toString y ++" and much more"

my_results: List String
my_results =
    [
       "\n IS lucky \n",
        pr <| lucky 4 ,
        "\n Say Me \n",
        pr <| sayMe 4 ,
        "\n factorial \n",
        pr <| factorial 5,
         "\n charName \n",
        pr <| charName 'a',
         "\n addVectors \n",
        pr <| addVectors (1 ,3)(4,5),
         "\n addVectors B \n",
        pr <| addVectors (1 ,3)(4,5),
         "\n Third B \n",
        pr <| third (1,2,3), 
        "\n Member Int \n",
        pr <| member 1 [1,2,3],
         "\n Member String \n",
        pr <| member "Hello" ["Hello","Hi","Hall"],
         "\n Tell \n",
        pr <| tell ["Hello","Hi","Hall"],
         "\n Length \n",
        pr <| length ["Hello","Hi","Hall"],
        "\n Sum \n",
        pr <| sum [1,2,3],
        "\n cylinder \n",
        pr <| cylinder 2 4

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