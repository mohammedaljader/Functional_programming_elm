module SomeAssignments4 exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)
import String exposing (toLower)
import String exposing (toList)
import Dict exposing (Dict)


--- isArmstrongNumber  ---- 

isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    let
        ( digits, nbDigits ) =
            digitsAndLength nb
    in
    nb == List.sum (List.map (\a -> a ^ nbDigits) digits)


digitsAndLength : Int -> ( List Int, Int )
digitsAndLength nb =
    if nb < 10 then
        ( [ nb ], 1 )

    else
        let
            ( digits, n ) =
                digitsAndLength (nb // 10)
        in
        ( modBy 10 nb :: digits, 1 + n )
---End isArmstrongNumber  ----

---- Anagram  --- 
detect : String -> List String -> List String
detect word candidates =
    let
        original =
            toLower word

        ref =
            normalize word
    in
    List.filter (\w -> normalize w == ref && toLower w /= original) candidates


normalize : String -> List Char
normalize word =
    word |> toLower |> toList |> List.sort
--- End Anagram  ---

--- Count Words ---
sanitizeSentence : String -> String
sanitizeSentence sentence =
     String.toLower sentence
        |>  String.map
            (\char ->
                if Char.isLower char || Char.isDigit char || char == '\'' then
                    char

                else
                    ' '
            )


sanitizeWord : String -> String
sanitizeWord word =
    if  String.startsWith "'" word then
        sanitizeWord ( String.dropLeft 1 word)

    else if  String.endsWith "'" word then
        sanitizeWord ( String.dropRight 1 word)

    else
        word


wordCount : String -> Dict String Int
wordCount sentence =
    sanitizeSentence sentence
        |>  String.words
        |> List.map sanitizeWord
        |> List.foldl
            (\word dict ->
                Dict.update
                    word
                    (\count -> Maybe.withDefault 0 count + 1 |> Just)
                    dict
            )
            Dict.empty
----End Words --- 

my_results: List String
my_results =
    [
       "\n isArmstrongNumber \n",
       "\n Single digit numbers are Armstrong numbers 5 \n",
       pr <| isArmstrongNumber 5,
       "\n Single digit numbers are Armstrong numbers 10 \n",
       pr <| isArmstrongNumber 10,
       "\n Single digit numbers are Armstrong numbers 153 \n",
       pr <| isArmstrongNumber 153,
         "\n Matching between 2 strings \n",
       "\n no matches \n",
       pr <| detect "diaper" [ "hello", "world", "zombies", "pants" ],
       "\n detects simple anagram \n",
       pr <| detect "ant" [ "tan", "stand", "at" ],
       "\n detects multiple anagrams \n",
       pr <| detect "solemn" [ "lemons", "cherry", "melons" ],
       "\n Words counter \n",
       "\ncount one of each word\n",
       pr <| Dict.toList(wordCount "one of each"),
       "\n multiple occurrences of a word \n",
       pr <| Dict.toList(wordCount "one fish two fish red fish blue fish"),
       "\n handles cramped lists \n",
       pr <| Dict.toList(wordCount "one,two,three")


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