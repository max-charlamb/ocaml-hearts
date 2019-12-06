open Partialdeck
open Card

module type BotSig = sig 

  val play : PartialDeck.t -> (card * int) list -> string -> bool -> card
  val lead : PartialDeck.t -> (card * int) list -> string -> card
  val pass : PartialDeck.t -> string -> card list

end

module Bot:BotSig = struct

  let suits = [Club; Diamond; Spade; Heart]

  (** [get_lowest suit hand] is the card with the lowest value in [suit]
      in [hand]. If there is no card of suit [suit], then the result is None. *)
  let get_lowest suit hand =
    match PartialDeck.lowest hand suit with 
    | exception CardNotFound -> None
    | c -> Some c 

  (** [get_highest suit hand] is the card with the highest value in [suit]
        in [hand]. If there is no card of suit [suit], then the result is 
        None. *)
  let get_highest suit hand =
    match PartialDeck.highest hand suit with 
    | exception CardNotFound -> None
    | c -> Some c 

  (** [get_new_suit acc suits] is a suit of Cards that is not already in [acc].
      If all suits are already in [acc], then raises Failure. *)
  let rec get_new_suit acc suits = 
    match suits with 
    | h :: t -> if List.mem h acc then get_new_suit acc t else h
    | [] -> failwith "empty hand"

  let rec play_easy hand pile suit suit_acc = 
    match get_lowest suit hand with 
    | None -> let acc = suit :: suit_acc in 
      let new_suit = get_new_suit acc suits in 
      play_easy hand pile new_suit acc
    | Some c -> c 

  (** [play_highest hand pile suit suit_acc] is the highest ranked card of
      [suit] in [hand]. *)
  and play_highest hand pile suit suit_acc = 
    match get_highest suit hand with 
    | None -> let acc = suit :: suit_acc in 
      let new_suit = get_new_suit acc suits in 
      play_easy hand pile new_suit acc
    | Some c -> c

  and play_med_helper hand pile queen_played queen_table = 
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in
    match queen_played, queen_table with 
    | true, false -> play_highest hand pile pile_suite []
    | true, true
    | false, true -> play_easy hand pile pile_suite []
    | false, false -> if List.length pile <> 3 
      then play_easy hand pile pile_suite []
      else begin 
        let c = play_highest hand pile pile_suite [] in   
        if c = {rank = Queen; suite = Spade} 
        then play_med_helper 
            (PartialDeck.remove {rank = Queen; suite = Spade} hand) 
            pile queen_played queen_table 
        else c 
      end 

  and play_med hand pile qspade qspadetable =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in 
    match pile_suite with 
    | Club -> play_easy hand pile pile_suite []
    | Diamond -> play_easy hand pile pile_suite []
    | Spade -> play_med_helper hand pile qspade qspadetable
    | Heart -> play_easy hand pile pile_suite []

  and play_hard_helper hand pile queen_played qspadetable = 
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in
    match queen_played, qspadetable with 
    | true, false -> play_highest hand pile pile_suite []
    | true, true
    | false, true -> play_easy hand pile pile_suite []
    | false, false -> if List.length pile = 3 
      then play_highest hand pile pile_suite [] 
      else play_easy hand pile pile_suite []

  and play_hard hand pile qspade qspadetable =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in 
    match pile_suite with 
    | Club -> play_easy hand pile pile_suite []
    | Diamond -> play_easy hand pile pile_suite []
    | Spade -> play_med_helper hand pile qspade qspadetable
    | Heart -> play_easy hand pile pile_suite []

  (** [list_to_deck pile acc] is the deck representation of the list of cards 
      in pile. *)
  and list_to_deck pile acc =   
    match pile with 
    | (card, _) :: t -> list_to_deck t (PartialDeck.insert card acc)
    | [] -> acc

  and play hand pile diff qs_played =
    let pile' = list_to_deck pile (PartialDeck.empty) in 
    let qs = {suite = Spade; rank = Queen} in 
    let qspadetable = (PartialDeck.mem qs pile') in 
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in
    match diff with 
    | "easy" -> play_easy hand pile pile_suite []
    | "medium" -> play_med hand pile qs_played qspadetable
    | "hard" -> play_easy hand pile pile_suite [] 
    | _ -> play_easy hand pile pile_suite []



  (* --------------------------------------------------------------------------- *)




  let rec lead_easy hand pile suit suit_acc = 
    match get_lowest suit hand with 
    | None -> let acc = suit :: suit_acc in 
      let new_suit = get_new_suit acc suits in 
      play_easy hand pile new_suit acc
    | Some c -> c 


  let lead_medium hand pile =
    match PartialDeck.highest hand Club with
    | exception CardNotFound ->
      begin 
        match PartialDeck.highest hand Diamond with
        | exception CardNotFound ->
          begin 
            match PartialDeck.lowest hand Spade with
            | exception CardNotFound ->
              begin 
                match PartialDeck.highest hand Heart with
                | exception CardNotFound ->
                  failwith "Hand has no cards"
                | c -> c
              end
            | c -> c
          end
        | c -> c
      end
    | c -> c

  let lead_hard hand pile =
    match PartialDeck.highest hand Club with
    | exception CardNotFound ->
      begin 
        match PartialDeck.highest hand Diamond with
        | exception CardNotFound ->
          begin 
            match PartialDeck.lowest hand Spade with
            | exception CardNotFound ->
              begin 
                match PartialDeck.lowest hand Heart with
                | exception CardNotFound ->
                  failwith "Hand has no cards"
                | c -> c
              end
            | c -> c
          end
        | c -> c
      end
    | c -> c

  let lead hand pile diff = 
    let twoclubs = {suite = Club; rank = Two} in 
    if PartialDeck.mem twoclubs hand then twoclubs else 
      match diff with 
      | "easy" -> lead_easy hand pile Club [] 
      | "medium" -> lead_medium hand pile
      | "hard" -> lead_hard hand pile
      | _ -> lead_easy hand pile Club []


  (* --------------------------------------------------------------------------- *)



  let pass_spades deck = 
    let qs = {suite=Spade; rank=Queen} in 
    let acesp = {suite=Spade; rank=Ace} in 
    let ks = {suite=Spade; rank=King} in 
    let in_hand = PartialDeck.mem qs deck, PartialDeck.mem ks deck,
                  PartialDeck.mem acesp deck in
    match in_hand with
    | true, true, true -> [qs; ks; acesp]
    | true, true, false -> [qs; ks]
    | true, false, false -> [qs]
    | false, true, true -> [acesp; ks]
    | false, false, false -> []
    | false, false, true -> [acesp]
    | false, true, false -> [ks]
    | true, false, true -> [qs; acesp]

  (** [pass_hearts deck count acc] is a list of the [count] highest hearts in 
      [deck]. If there are no hearts in [deck], the result is [acc]. *)
  let rec pass_hearts deck count acc = 
    if count = 0 then acc else 
      match PartialDeck.highest deck Heart with 
      | exception CardNotFound -> acc
      | c -> let new_hand = PartialDeck.remove c deck in 
        pass_hearts new_hand (count - 1) (c::acc)

  (** [pass_diamonds_clubs deck count acc] is a list of the [count] highest 
      clubs in [deck]. If there are no clubs in [deck], the result is a list 
      of the highest diamonds. *)
  let rec pass_diamonds_clubs deck count acc = 
    if count = 0 then acc else 
      match PartialDeck.highest deck Club with 
      | c -> let new_hand = PartialDeck.remove c deck in 
        pass_diamonds_clubs new_hand (count - 1) (c::acc)
      | exception CardNotFound -> begin 
          match PartialDeck.highest deck Diamond with 
          | c -> let new_hand = PartialDeck.remove c deck in 
            pass_diamonds_clubs new_hand (count - 1) (c::acc)
          | exception CardNotFound -> acc
        end

  (** [pass_easy deck acc] is a list of three unique random cards in [deck]. *)
  let rec pass_easy deck acc = 
    if List.length acc = 3 then acc else 
      match PartialDeck.random_card deck with 
      | None -> pass_easy deck acc
      | Some c -> if List.mem c acc then pass_easy deck acc 
        else pass_easy deck (c :: acc)

  (** [pass_med deck suit suit_acc suits acc] is a list of three cards to pass. 
      The list of cards selected consists of the highest [suit] cards. If there
      are no cards of [suit], then the highest cards of a different random 
      suit are selected. *)
  let rec pass_med deck suit suit_acc suits acc = 
    if List.length acc = 3 then acc else 
      match get_highest suit deck with 
      | None -> let sacc = suit :: suit_acc in 
        let new_suit = get_new_suit sacc suits in 
        pass_med deck new_suit sacc suits acc
      | Some c -> let deck' = PartialDeck.remove c deck in 
        pass_med deck' suit suit_acc suits (c :: acc)

  (** [pass_hard deck] is the list of three cards to pass. If [deck] contains 
      the Queen, King, or Ace of Spades, these cards are in the list. The 
      remaining, if any, spots in the list of three cards consists of the 
      highest hearts and then clubs/diamonds. *)
  let pass_hard deck = 
    let hrts_spds = 
      match pass_spades deck with 
      | a :: b :: c :: [] -> a :: b :: c :: []
      | a :: b :: [] -> a :: b :: pass_hearts deck 1 []
      | a :: [] -> a :: pass_hearts deck 2 []
      | [] -> pass_hearts deck 3 []
      | _ -> failwith "Not enough cards"
    in 
    match hrts_spds with 
    | a :: b :: c :: [] -> [a; b; c] 
    | a :: b :: [] -> a :: b :: pass_diamonds_clubs deck 1 []
    | a :: [] -> a :: pass_diamonds_clubs deck 2 []
    | [] -> pass_diamonds_clubs deck 3 []
    | _ -> failwith "Not enough cards"

  (** [random_suit suits] is a random suit from [suits].
      Requires: [suits] is not empty. *)
  let random_suit suits = 
    Random.self_init ();
    (List.nth suits (Random.int (List.length suits)))

  let pass deck difficulty = 
    match difficulty with 
    | "easy" -> pass_easy deck []
    | "medium" -> pass_med deck (random_suit suits) [] suits []
    | "hard" -> let out = pass_hard deck in 
      if List.length out < 3 then failwith "Not enough cards" else out
    | _ -> pass_easy deck []

end