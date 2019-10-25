open OUnit2
open Partialdeck

let deck1 = PartialDeck.empty |> PartialDeck.insert (Card(Spade, Queen))
            |> PartialDeck.insert (Card(Spade, King))
let deck2 = PartialDeck.empty |> PartialDeck.insert (Card(Spade, Queen))
let deck3 = PartialDeck.empty |> PartialDeck.insert (Card(Spade, King))


let partialdecktests = [
  "empty" >:: (fun _ -> 
      assert_equal 0 (PartialDeck.empty |> PartialDeck.size));
  "full" >:: (fun _ ->
      assert_equal 52 (PartialDeck.full |> PartialDeck.size));
  "insert" >:: (fun _ ->
      assert_equal [Card (Spade, Queen)]
        (PartialDeck.empty |> PartialDeck.insert (Card(Spade, Queen)) 
         |> PartialDeck.to_list));
  "move" >:: (fun _ ->
      assert_equal (deck2,deck3) 
        (PartialDeck.move (Card(Spade,King)) deck1 PartialDeck.empty));

]

let suite =
  "test suite for Hearts"  >::: List.flatten [
    partialdecktests;
  ]

let _ = run_test_tt_main suite
