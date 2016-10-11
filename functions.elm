import Html exposing (text, div)

four : Float
four =
  sqrt 16

eleven : Float
eleven =
  max 2 11

twenty : Float
twenty =
  max (sqrt 100) (4 * 5)

factorial : Float -> Float
factorial n =
  List.product [1..n]

factorial2 :  Float -> Float
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

zip : List a -> List b -> List (a, b)
zip xs ys =
  case (xs, ys) of
    (x :: xs', y :: ys') ->
      (x, y) :: zip xs' ys'
    (_, _) ->
      []

unzip : List (a, b) -> (List a, List b)
unzip list =
  case list of
    [] ->
      ([], [])
    (xs, ys) :: rest ->
      let
        (xs', ys') = unzip rest
      in
        (xs :: xs', ys :: ys')

quicksort : List comparable -> List comparable
quicksort list =
  case list of
    [] ->
      []
    pivot :: rest ->
      let
        (lower, higher) = List.partition (\n -> n <= pivot) rest
      in
        quicksort lower ++ [pivot] ++ quicksort higher

mergesort : List comparable -> List comparable
mergesort list =
  case list of
    [] ->
      list
    [_] ->
      list
    _ ->
    let
      (xs, ys) = split list
    in
      merge (mergesort xs) (mergesort ys)

split : List a -> (List a, List a)
split list =
  case list of
    [] ->
      ([], [])
    x :: rest ->
      let
        (xs, ys) = split rest
      in
        (ys, x :: xs)

merge : List comparable -> List comparable -> List comparable
merge xs ys =
  case (xs, ys) of
    (x :: xs', y :: ys') ->
      if x < y
        then x :: merge xs' ys
        else y :: merge xs ys'
    ([], rest) ->
      rest
    (rest, []) ->
      rest

main : Html.Html a
main =
  div []
    [ div [] [text (toString [four, eleven, twenty, factorial 4, factorial2 4, listLength [1..7]])]
    , div []
      [ zip [1..5] [5..10]
      |> toString
      |> text
      ]
    , div []
      [ zip [1..5] [5..10]
      |> unzip
      |> toString
      |> text
      ]
    , div []
        [ quicksort [5,4,3,2,1]
        |> toString
        |> text
        ]
    , div []
        [ mergesort [5,4,7,3,9,1]
        |> toString
        |> text
        ]
    ]