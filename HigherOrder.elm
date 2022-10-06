module HigherOrder exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Html exposing (s)
import Html.Attributes exposing (list)
import Html exposing (b)
import Tuple exposing (..)
import String exposing (fromFloat)

----code----
type Function
  = Poly Function Float
  | Mult Function Function
  | Div Function Function
  | Plus Function Function
  | Minus Function Function
  | Const Float
  | X 


print : Function -> String
print expr = 
   case expr of
       Mult a b ->
         "(" ++ print a ++ "*" ++ print b ++ ")"
       Plus a b ->
         "(" ++ print a ++ "+" ++ print b ++ ")"
       Minus a  b ->
         "(" ++ print a ++ "-" ++ print b ++ ")"
       Poly a b ->
         "(" ++ print a ++ "^" ++ fromFloat b ++ ")"
       Div a b ->
         "(" ++ print a ++ "/" ++ print b ++ ")"
       Const a ->
         fromFloat a
       X ->
        "x"

eval : Float -> Function -> Float
eval y expr = 
   case expr of 
     Mult a b ->
        eval y a * eval y b
     Plus a b ->
        eval y a + eval y b
     Minus a b ->
        eval y a - eval y b
     Poly a b ->
        eval y a ^ b
     Div a b ->
        eval y a / eval y b
     Const a ->
         a
     X ->
      y


-- this function is used to calculate y-axis values
-- which will be used to store the needed values to generate the graph
calculateYValues: Function -> Float -> Float -> List Float
calculateYValues expr minX maxX= 
    if(minX < maxX) then
       eval minX expr :: calculateYValues expr (minX + 1) maxX
    else
      []


-- this function keeps generating lines with either *, - or both
-- depending on the result given by the function evaluation (used in the graph function)
-- we check the following conditions to generate a line :
-- -> if the y_min is smaller than the function evalution and we are still in the range,
--    then we put a * 
--    else if we are still within the range, then put a - 

line : Float -> Float -> Float -> String
line value upper_range current = 
  if current < value && current <= upper_range then
    "*" ++ line value upper_range (current + 1)
  else 
    if current <= upper_range then
      "-" ++ line value upper_range (current + 1)
    else
      ""

-- function used for creating the graph for the function
-- the list of float stores the results obtained from the function evaluations
-- for each element of the list, we draw a line according to what String the "line" function returns
graph : List Float -> Float -> Float -> Float -> Float -> String
graph ls x_min x_max y_min y_max = 
      case ls of 
      [] ->
        ""
      x :: xs -> 
       line x y_max y_min ++ "\n" ++ graph xs x_min x_max y_min y_max
       

-- collecting results for printing:

f : Function
f = Plus (Minus (Poly (Minus (Div (X) (Const 5)) (Const 1)) 4) (Poly (Plus (Div (X) (Const -2)) (Const 2)) 2))(Const 6)
f2 : Function
f2 = (Div (Poly X 4) (Const 10))

lst = calculateYValues f -10 20
ls2 = calculateYValues f2 -10 20

my_results: List String
my_results =
    [
       pr <| print f,
       graph lst -10 20 -10 10, 
       "\n",
       pr <| print f2,
       graph ls2 -10 10 -10 10

    ] 
    
-- Boiler-plate below:
-- update this values for long output lines, or small browser windows
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