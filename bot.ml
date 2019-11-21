open Partialdeck
open Card

module type BotSig = sig 

  val play : PartialDeck.t -> (card * int) list -> string -> card
  val lead : PartialDeck.t -> (card * int) list -> string -> card
  val pass : PartialDeck.t -> (card * int) list -> card list

end

module Bot:BotSig = struct
  let suits = [Club; Diamond; Spade; Heart]

  let get_lowest suit hand =
    match PartialDeck.lowest hand suit with 
    | exception CardNotFound -> None
    | c -> Some c 

  let get_highest suit hand =
    match PartialDeck.highest hand suit with 
    | exception CardNotFound -> None
    | c -> Some c 

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
    | false, false -> if List.length pile = 3 then play_highest hand pile pile_suite []
      else play_easy hand pile pile_suite []


  and play_med hand pile qspade qspadetable =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in 
    match pile_suite with 
    | Club -> play_easy hand pile pile_suite []
    | Diamond -> play_easy hand pile pile_suite []
    | Spade -> play_med_helper hand pile qspade qspadetable
    | Heart -> play_easy hand pile pile_suite []

  and play_hard_helper hand pile queen_played queen_table = 
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in
    match queen_played, queen_table with 
    | true, false -> play_highest hand pile pile_suite []
    | true, true
    | false, true -> play_easy hand pile pile_suite []
    | false, false -> if List.length pile = 3 then play_highest hand pile pile_suite [] 
      else play_easy hand pile pile_suite []


  and play_hard hand pile qspade qspadetable =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in 
    match pile_suite with 
    | Club -> play_easy hand pile pile_suite []
    | Diamond -> play_easy hand pile pile_suite []
    | Spade -> play_med_helper hand pile qspade qspadetable
    | Heart -> play_easy hand pile pile_suite []

  and list_to_deck pile acc =   
    match pile with 
    | (card, _) :: t -> list_to_deck t (PartialDeck.insert card acc)
    | [] -> acc

  and play hand pile diff =
    let pile' = list_to_deck pile (PartialDeck.empty) in 
    let qs = {suite = Spade; rank = Queen} in 
    let qspadetable = (PartialDeck.mem qs pile') in 
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in
    match diff with 
    | "easy" -> play_easy hand pile pile_suite []
    | "medium" -> play_med hand pile true qspadetable
    | "hard" -> play_easy hand pile pile_suite [] 
    | _ -> play_easy hand pile pile_suite []

  let lead_easy hand pile =
    match PartialDeck.lowest hand Club with
    | exception CardNotFound ->
      begin 
        match PartialDeck.lowest hand Diamond with
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
    match diff with 
    | "easy" -> lead_easy hand pile
    | "medium" -> lead_medium hand pile
    | "hard" -> lead_hard hand pile
    | _ -> lead_easy hand pile

  let pass deck pile = 
    failwith "uni"

end