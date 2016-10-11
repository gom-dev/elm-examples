import Html exposing (text)

four =
  sqrt 16

eleven =
  max 2 11

twenty =
  max (sqrt 100) (4 * 5)

factorial n =
  List.product [1..n]

factorial2 n =
  if n == 1 then
    1
  else
    n * (factorial2 (n-1))

listLength: List a -> Float
listLength list =
  case list of
    [] ->
      0
    first :: rest ->
      1 + listLength rest


main =
  text (toString [four, eleven, twenty, factorial 4, factorial2 4, listLength [1..7]])