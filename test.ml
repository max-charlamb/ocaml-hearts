open OUnit2
open Card
open Partialdeck
open Command

(* Card Tests *)

let king_spade = {suite=Spade; rank=King}
let queen_spade = {suite=Spade; rank=Queen}
let queen_heart = {suite=Heart; rank=Queen}

let cardtests = [
  "compare01" >:: (fun _ -> assert_equal 1 (compare king_spade queen_spade));
  "compare02" >::(fun _ -> assert_equal (-1) (compare queen_spade king_spade));
  "compare03" >::(fun _ -> assert_equal (-1) (compare queen_heart king_spade));
  "compare04" >::(fun _ -> assert_equal (-1) (compare queen_heart queen_spade));
]

(* Partial Deck Tests *)

let deck1 = PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=Queen}
            |> PartialDeck.insert {suite=Spade; rank=King}
let deck2 = PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=Queen}
let deck3 = PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=King}

let partialdecktests = [
  "empty" >:: (fun _ -> 
      assert_equal 0 (PartialDeck.empty |> PartialDeck.size));
  "full" >:: (fun _ ->
      assert_equal 52 (PartialDeck.full |> PartialDeck.size));
  "insert" >:: (fun _ ->
      assert_equal [{suite=Spade; rank=Queen}]
        (PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=Queen} 
         |> PartialDeck.to_list));
  "move" >:: (fun _ ->
      assert_equal (deck2,deck3) 
        (PartialDeck.move {suite=Spade; rank=King} deck1 PartialDeck.empty));
]

(* Command Tests *)

let commandtests = [
  "pass" >:: (fun _ -> 
      assert_equal (Pass(1,2,3)) (parse "pass 1 2 3"));
  "empty" >:: (fun _ -> assert_raises Empty (fun () -> (parse "")));
  "malformed01" >:: (fun _ -> assert_raises Malformed (fun () -> (parse "test")));
  "malformed02" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "pass 1 2 3 4")));
  "malformed03" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "play t")));
]


let suite =
  "test suite for Hearts"  >::: List.flatten [
    cardtests;
    partialdecktests;
    commandtests;
  ]

let _ = run_test_tt_main suite
