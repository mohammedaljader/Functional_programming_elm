module SomeAssignments2 exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Tuple exposing (..)
import String exposing (contains, fromChar, toLower)
import Dict
import Dict exposing (Dict)

--raindrops  --
raindrops : Int -> String
raindrops number =
    let
        drops =
            [ if modBy 3 number == 0 then
                "Pling"

              else
                ""
            , if modBy 5 number == 0 then
                "Plang"

              else
                ""
            , if modBy 7 number == 0 then
                "Plong"

              else
                ""
            ]

        result =
             String.join "" drops
    in
    if result == "" then
         String.fromInt number

    else
        result
--- End raindrops  ---- 

---  triplets --- 
type alias Triplet =
    ( Int, Int, Int )


{-| For an integer n, find all triplets a,b,c such that:
a < b < c
a + b + c = n
a^2 + b^2 = c^2
-}
triplets : Int -> List Triplet
triplets n =
    -- 3 is the range start since 3,4,5 is the lowest Pythagorean triplet.
    -- n // 3 is the range end since we have a < b < c and a + b + c = n.
    List.range 3 (n // 3)
        |> List.filterMap (computeTriplet n)


{-| Given n and a, find the value for b and c that complete the a,b,c triplet.
-}
computeTriplet : Int -> Int -> Maybe Triplet
computeTriplet n a =
    -- From the equations:
    --    a + b + c = n        (1)
    --    a^2 + b^2 = c^2      (2)
    -- we can replace c by (n - a - b) and inject it in (2) to find that
    --    b = n(n-2a) / 2(n-a)
    -- which is only valid if b > a and b is an integer.
    let
        num =
            n * (n - 2 * a)

        denum =
            2 * (n - a)

        b =
            num // denum
    in
    if b > a && b * denum == num then
        Just ( a, b, n - a - b )

    else
        Nothing

---  End triplets ---

--- Next row --- 
nextRow : List Int -> List Int
nextRow row =
    List.map2 (+) (0 :: row) (row ++ [ 0 ])


rows : Int -> List (List Int)
rows n =
    let
        loop i row =
            if i == n then
                []

            else
                row :: loop (i + 1) (nextRow row)
    in
    if n < 0 then
        []

    else
        loop 0 [ 1 ]

--- End Row  ---

--- isPangram  --- 

isPangram : String -> Bool
isPangram sentence =
    let
        normalized =
            toLower sentence
    in
     String.all (\c -> contains (fromChar c) normalized) alphabet

---(End isPangram)----

--- IsPaired ---
isPaired : String -> Bool
isPaired input =
    let
        onlyBrackets =
             String.filter (\a -> List.member a bracketList)

        tryMatching bracket visited =
            case visited of
                [] ->
                    bracket :: visited

                top :: visited_ ->
                    if isPairMatch top bracket then
                        visited_

                    else
                        bracket :: top :: visited_

        isPairMatch prev current =
            case Dict.get prev bracketDict of
                Nothing ->
                    False

                Just rightBracket ->
                    current == rightBracket
    in
    input
        |> onlyBrackets
        |>  String.foldl tryMatching []
        |> List.isEmpty


bracketList : List Char
bracketList =
    [ '{', '}', '[', ']', '(', ')' ]


bracketDict : Dict Char Char
bracketDict =
    Dict.fromList
        [ ( '{', '}' )
        , ( '[', ']' )
        , ( '(', ')' )
        ]


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b

--- End Ispaired --- 


alphabet : String
alphabet =
    "abcdefghijklmnopqrstuvwxyz"

my_results: List String
my_results =
    [
       "\n ---(raindrops)---- \n",
       "\n raindrops 105 \n",
       pr <| raindrops 105,
       "\n raindrops 2 \n",
       pr <| raindrops 3,
       "\n ---(triplets)---- \n",
       "\n triplets 12  \n",
       pr <| triplets 12,
       "\n triplets 90  \n",
       pr <| triplets 90,
       "\n ---(Row)---- \n",
       "\n rows 3 \n",
       pr <|rows 3,
       "\n rows 4 \n",
       pr <|rows 4,
       "\n rows -1 \n",
       pr <|rows -1,
       "\n ---(isPangram)---- \n",
       "\n isPangram the quick brown fox jumps over the lazy dog \n",
       pr <| isPangram "the quick brown fox jumps over the lazy dog",
       "\n isPangram a quick movement of the enemy will jeopardize five gunboats \n",
       pr <| isPangram "a quick movement of the enemy will jeopardize five gunboats",
        "\n ---(isPangram)---- \n",
         "\n isPaired }{ \n",
       pr <| isPaired "}{",
          "\n isPaired mm \n",
       pr <| isPaired "mm"
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