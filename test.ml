open OUnit2
open Card
open Partialdeck
open Command
open Round
open Bot

(* Test Plan
   We tested the backend of our system using OUnit tests. This includes the
   modules Card, Partialdeck, Command, and some of Round. This was done because
   the rest of the modules require IO and are difficult to test here. For the 
   most part we use Black Box testing and make sure that our functions are able 
   to pass all the edge cases we can think of. This approach is able to test
   the correctness of the system because the backend is correct, the frontend is 
   easily testable in play testing. The few test cases we added for round 
   rely on running functions testing expected behavior rather than the 
   exact state of the round. Testing the exact state would be difficult
   due to the random nature of the bots. 
   We used Bisect to ensure most of our code is covered. We bisect ignored 
   functions having to do with printing as these are tested visually. 
*)

(* Card Tests *)

let king_spade = {suite=Spade; rank=King}
let queen_spade = {suite=Spade; rank=Queen}
let queen_heart = {suite=Heart; rank=Queen}

let cardtests = [
  "compare01" >:: (fun _ -> assert_equal 1 (compare king_spade queen_spade));
  "compare02" >::(fun _ -> assert_equal (-1) (compare queen_spade king_spade));
  "compare03" >::(fun _ -> assert_equal (-1) (compare queen_heart king_spade));
  "compare04" >::(fun _ -> assert_equal (-1) (compare queen_heart queen_spade));
  "compare05" >:: (fun _ -> assert_raises DuplicateCard 
                      (fun () -> compare queen_heart queen_heart));
]

(* Partial Deck Tests *)

let deck1 = PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=Queen}
            |> PartialDeck.insert {suite=Spade; rank=King}
let deck2 = PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=Queen}
let deck3 = PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=King}
let deck4 = PartialDeck.full
let deck5 = PartialDeck.empty |> PartialDeck.insert {suite=Heart; rank=Two}
let deck6 = PartialDeck.empty |> PartialDeck.add_cards [
    {rank=Two; suite=Heart};
    {rank=Three; suite=Heart};
    {rank=Four; suite=Heart};
    {rank=Five; suite=Heart};
    {rank=Six; suite=Heart};
    {rank=Seven; suite=Heart};
    {rank=Eight; suite=Heart};
    {rank=Nine; suite=Heart};
    {rank=Ten; suite=Heart};
    {rank=Jack; suite=Heart};
    {rank=Queen; suite=Heart};
    {rank=King; suite=Heart};
    {rank=Ace; suite=Heart};
    {rank=Queen; suite=Spade};
  ]
let deck7 = PartialDeck.empty |> PartialDeck.add_cards [
    {rank=Two; suite=Heart};
    {rank=Three; suite=Heart};
    {rank=Four; suite=Heart};
    {rank=Five; suite=Heart};
    {rank=Six; suite=Heart};
    {rank=Seven; suite=Heart};
    {rank=Eight; suite=Heart};
    {rank=Jack; suite=Heart};
    {rank=Queen; suite=Heart};
    {rank=King; suite=Heart};
    {rank=Ace; suite=Heart};
    {rank=Queen; suite=Spade};
  ]

let partialdecktests = [
  "empty" >:: (fun _ -> 
      assert_equal 0 (PartialDeck.empty |> PartialDeck.size));
  "empty02" >:: (fun _ -> 
      assert_equal true (PartialDeck.empty |> PartialDeck.is_empty));
  "full" >:: (fun _ ->
      assert_equal 52 (PartialDeck.full |> PartialDeck.size));
  "insert01" >:: (fun _ ->
      assert_equal [({suite=Spade; rank=Queen},1)]
        (PartialDeck.empty |> PartialDeck.insert {suite=Spade; rank=Queen} 
         |> PartialDeck.to_list));
  "insert02" >:: (fun _ ->
      assert_raises DuplicateCard
        (fun () -> PartialDeck.empty |> 
                   PartialDeck.insert {suite=Spade; rank=Queen} |>
                   PartialDeck.insert {suite=Spade; rank=Queen} ));
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
  "shoot_the_moon01" >:: (fun _ ->
      assert_equal (PartialDeck.shoot_the_moon deck6) true);
  "shoot_the_moon02" >:: (fun _ ->
      assert_equal (PartialDeck.shoot_the_moon deck1) false);
  "shoot_the_moon03" >:: (fun _ ->
      assert_equal (PartialDeck.shoot_the_moon 
                      (PartialDeck.insert 
                         {suite=Spade; rank=Three} deck6)) true);
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
                     (PartialDeck.mem {suite = Heart; rank = King} deck1) false);
  "highest not found" >:: (fun _ -> assert_raises CardNotFound 
                              (fun () -> (PartialDeck.highest deck6 Club)));
  "lowest not found" >:: (fun _ -> assert_raises CardNotFound 
                             (fun () -> (PartialDeck.lowest deck6 Club)));
  "remove01" >:: (fun _ -> 
      assert_raises CardNotFound (fun () -> 
          deck6 |> PartialDeck.remove {suite=Spade; rank=Three}));
  "find01" >:: (fun _ -> 
      assert_equal (Some{rank=Three; suite=Heart}) 
        (deck6 |> PartialDeck.find 2));
  "find02" >:: (fun _ -> 
      assert_equal None 
        (deck6 |> PartialDeck.find 30));
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
  "select" >:: (fun _ -> 
      assert_equal (Select("easy")) (parse "select easy"));
  "back" >:: (fun _ -> 
      assert_equal (Back) (parse "back"));
  "debug" >:: (fun _ -> 
      assert_equal (Debug) (parse "debug"));
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
  "malformed05" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "select")));
  "malformed06" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "pass 1 2 2"));
  "malformed07" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "back 3")));
  "malformed08" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "quit 324")));
  "malformed09" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "play 2 3")));
  "malformed10" >:: (fun _ -> assert_raises Malformed 
                        (fun () -> (parse "select easy hard")));

]

(* Round Tests *)

let newround = match Round.new_round Easy "test" |> Round.deal with 
  | Valid t -> t
  | _ -> failwith ""

let all_hearts = (PartialDeck.add_cards [
    {rank=Two; suite=Heart};
    {rank=Three; suite=Heart};
    {rank=Four; suite=Heart};
    {rank=Five; suite=Heart};
    {rank=Six; suite=Heart};
    {rank=Seven; suite=Heart};
    {rank=Eight; suite=Heart};
    {rank=Nine; suite=Heart};
    {rank=Ten; suite=Heart};
    {rank=Jack; suite=Heart};
    {rank=Queen; suite=Heart};
    {rank=King; suite=Heart};
    {rank=Ace; suite=Heart};
  ] PartialDeck.empty)

let all_spades = (PartialDeck.add_cards [
    {rank=Two; suite=Spade};
    {rank=Three; suite=Spade};
    {rank=Four; suite=Spade};
    {rank=Five; suite=Spade};
    {rank=Six; suite=Spade};
    {rank=Seven; suite=Spade};
    {rank=Eight; suite=Spade};
    {rank=Nine; suite=Spade};
    {rank=Ten; suite=Spade};
    {rank=Jack; suite=Spade};
    {rank=Queen; suite=Spade};
    {rank=King; suite=Spade};
    {rank=Ace; suite=Spade};
  ] PartialDeck.empty)

let all_diamonds = (PartialDeck.add_cards [
    {rank=Two; suite=Diamond};
    {rank=Three; suite=Diamond};
    {rank=Four; suite=Diamond};
    {rank=Five; suite=Diamond};
    {rank=Six; suite=Diamond};
    {rank=Seven; suite=Diamond};
    {rank=Eight; suite=Diamond};
    {rank=Nine; suite=Diamond};
    {rank=Ten; suite=Diamond};
    {rank=Jack; suite=Diamond};
    {rank=Queen; suite=Diamond};
    {rank=King; suite=Diamond};
    {rank=Ace; suite=Diamond};
  ] PartialDeck.empty)

let all_clubs = (PartialDeck.add_cards [
    {rank=Two; suite=Club};
    {rank=Three; suite=Club};
    {rank=Four; suite=Club};
    {rank=Five; suite=Club};
    {rank=Six; suite=Club};
    {rank=Seven; suite=Club};
    {rank=Eight; suite=Club};
    {rank=Nine; suite=Club};
    {rank=Ten; suite=Club};
    {rank=Jack; suite=Club};
    {rank=Queen; suite=Club};
    {rank=King; suite=Club};
    {rank=Ace; suite=Club};
  ] PartialDeck.empty)

let testround1 = Round.test_deal [
    all_clubs;
    all_hearts;
    all_spades;
    all_diamonds;
  ]

let testround2 = Round.test_deal [
    all_hearts;
    all_clubs;
    all_spades;
    all_diamonds;
  ]

let roundtests = [
  "player hand size" >:: (fun _ -> 
      assert_equal 13 (PartialDeck.size (Round.hand newround)));
  "assert wrong card" >:: (fun _ -> 
      assert_equal false 
        begin 
          match (Round.play {rank=Ten;suite=Spade} newround) with 
          | Invalid(s) -> false
          | Valid (t) -> true
        end);
  "holding 2 clubs first round play it" >:: (fun _ -> 
      assert_equal "Play" 
        begin 
          match (Round.play {rank=Two;suite=Club} testround1) with 
          | Invalid(s) -> "fail"
          | Valid (t) -> t |> Round.next_action
        end ~printer:(fun x -> x));
  "holding 2 clubs first round does not play it" >:: (fun _ -> 
      assert_equal "You must play the two of clubs!" 
        begin 
          match (Round.play {rank=Three;suite=Club} testround1) with 
          | Invalid(s) -> s
          | Valid (t) -> t |> Round.next_action
        end ~printer:(fun x -> x));
  "not holding 2 clubs first round" >:: (fun _ -> 
      assert_equal "Play" 
        begin 
          match (Round.play {rank=Two;suite=Heart} testround2) with 
          | Invalid(s) -> "fail"
          | Valid (t) -> t |> Round.next_action
        end ~printer:(fun x -> x));
  "full game shoot the moon" >:: (fun _ -> 
      assert_equal [-10;26;26;26]
        begin 
          (List.fold_left (fun round _ -> 
               begin 
                 let first_card = 
                   match (PartialDeck.find 1 (Round.bot_hand round 0)) with 
                   | Some (c) -> c
                   | None -> {rank=Two;suite=Heart}
                 in
                 match Round.play first_card round with 
                 | Invalid(s) -> round
                 | Valid (r) -> r
               end
             ) testround1 [1;2;3;4;5;6;7;8;9;10;11;12;13])|> Round.total_score
        end ~printer:(fun x -> List.fold_left 
                         (fun acc y -> acc ^ string_of_int y ^ "; " ) "" x));
]

(* Bot test *)
let handbot = Round.bot_hand newround 2
let handbot2 = Round.bot_hand newround 1
let bottests = [
  "pass_easy" >:: (fun _ -> assert_equal 3 
                      (List.length (Bot.pass handbot "easy")));
  "pass_medium" >:: (fun _ -> assert_equal 3 
                        (List.length (Bot.pass handbot "medium")));
  "pass_hard" >:: (fun _ -> assert_equal 3 
                      (List.length (Bot.pass handbot "hard")));
  "pass_easy" >:: (fun _ -> assert_equal 3 
                      (List.length (Bot.pass handbot2 "easy")));
  "pass_medium" >:: (fun _ -> assert_equal 3 
                        (List.length (Bot.pass handbot2 "medium")));
  "pass_hard" >:: (fun _ -> assert_equal 3 
                      (List.length (Bot.pass handbot2 "hard")));
]

let suite =
  "test suite for Hearts"  >::: List.flatten [
    cardtests;
    partialdecktests;
    commandtests;
    roundtests;
    bottests;
  ]

let _ = run_test_tt_main suite
