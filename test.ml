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
let deck4 = PartialDeck.full
let deck5 = PartialDeck.empty |> PartialDeck.insert {suite=Heart; rank=Two}

let partialdecktests = [
  "empty" >:: (fun _ -> 
      assert_equal 0 (PartialDeck.empty |> PartialDeck.size));
  "full" >:: (fun _ ->
      assert_equal 52 (PartialDeck.full |> PartialDeck.size));
  "insert" >:: (fun _ ->
      assert_equal [({suite=Spade; rank=Queen},1)]
        (PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=Queen} 
         |> PartialDeck.to_list));
  "move" >:: (fun _ ->
      assert_equal (deck2,deck3) 
        (PartialDeck.move {suite=Spade; rank=King} deck1 PartialDeck.empty));
  "to_list01" >:: (fun _ ->
      assert_equal 2 (List.length (PartialDeck.to_list deck1)));
  "to_list02" >:: (fun _ ->
      assert_equal 52 (List.length (PartialDeck.to_list deck4)));
  "count_points01" >:: (fun _ ->
      assert_equal (PartialDeck.count_points PartialDeck.full) 16
        ~printer:string_of_int);
  "count_points02" >:: (fun _ ->
      assert_equal (PartialDeck.count_points deck3) 0
        ~printer:string_of_int);
  "merge01" >:: (fun _ ->
      assert_equal deck1 (PartialDeck.merge deck2 deck3)
        ~printer:PartialDeck.string_of_partialdeck);
  "contains_hearts01" >:: (fun _ ->
      assert_equal (PartialDeck.contains_hearts deck5) true);
  "contains_hearts02" >:: (fun _ ->
      assert_equal (PartialDeck.contains_hearts deck1) false);
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
