open OUnit2
open Card
open Partialdeck
open Command
open Round

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
  "empty02" >:: (fun _ -> 
      assert_equal true (PartialDeck.empty |> PartialDeck.is_empty));
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
  "lowest" >:: (fun _ ->
      assert_equal (PartialDeck.lowest PartialDeck.full Diamond) 
        ({suite = Diamond; rank = Two}));
  "highest" >:: (fun _ ->
      assert_equal (PartialDeck.highest PartialDeck.full Heart) 
        ({suite = Heart; rank = Ace}));
  "voided01" >:: (fun _ -> assert_equal (PartialDeck.voided Spade deck1) false);
  "voided02" >:: (fun _ -> assert_equal (PartialDeck.voided Heart deck1) true);
  "member01" >:: (fun _ -> assert_equal 
                     (PartialDeck.mem {suite = Spade; rank = King} deck1) true);
  "member02" >:: (fun _ -> assert_equal 
                     (PartialDeck.mem {suite = Heart; rank = King} deck1) false)
]

(* Command Tests *)

let commandtests = [
  "pass" >:: (fun _ -> 
      assert_equal (Pass(1,2,3)) (parse "pass 1 2 3"));
  "play" >:: (fun _ -> 
      assert_equal (Play (2)) (parse "play 2"));
  "quit" >:: (fun _ -> 
      assert_equal (Quit) (parse "quit  "));
  "help" >:: (fun _ -> 
      assert_equal (Help) (parse "help"));
  "restart" >:: (fun _ -> 
      assert_equal (Restart) (parse "restart"));
  "deal" >:: (fun _ -> 
      assert_equal (Deal) (parse "deal"));
  "empty01" >:: (fun _ -> assert_raises Empty (fun () -> (parse "")));
  "empty02" >:: (fun _ -> assert_raises Empty (fun () -> (parse "  ")));
  "malformed01" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "test")));
  "malformed02" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "pass 1 2 3 4")));
  "malformed03" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "play t")));
  "malformed04" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "quit 324")));
]

let newround = match Round.new_round Easy |> Round.deal with 
  | Valid t -> t
  | _ -> failwith ""

let roundtests = [
  "player hand size" >:: (fun _ -> assert_equal 13 (PartialDeck.size (Round.hand newround)))
]

let suite =
  "test suite for Hearts"  >::: List.flatten [
    cardtests;
    partialdecktests;
    commandtests;
    roundtests;
  ]

let _ = run_test_tt_main suite
