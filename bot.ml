open Partialdeck
open Card

module type BotSig = sig 

  val play : PartialDeck.t -> (card * int) list -> string -> card
  val lead : PartialDeck.t -> (card * int) list -> string -> card
  val pass : PartialDeck.t -> (card * int) list -> card list

end

module Bot:BotSig = struct

  let rec play_easy hand pile =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in
    match PartialDeck.lowest hand pile_suite with
    | exception CardNotFound -> 
      begin
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
      end 
    | c -> c

  and play_highest hand pile = 
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in
    match PartialDeck.highest hand pile_suite with
    | exception CardNotFound -> 
      begin
        match PartialDeck.highest hand Club with
        | exception CardNotFound ->
          begin 
            match PartialDeck.highest hand Diamond with
            | exception CardNotFound ->
              begin 
                match PartialDeck.highest hand Spade with
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
      end 
    | c -> c

  and play_med_helper hand pile queen_played queen_table = 
    match queen_played, queen_table with 
    | true, false -> play_highest hand pile
    | true, true
    | false, true -> play_easy hand pile
    | false, false -> if List.length pile = 3 then play_highest hand pile 
      else play_easy hand pile


  and play_med hand pile qspade qspadetable =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in 
    match pile_suite with 
    | Club -> play_easy hand pile 
    | Diamond -> play_easy hand pile
    | Spade -> play_med_helper hand pile qspade qspadetable
    | Heart -> play_easy hand pile

  and play_hard_helper hand pile queen_played queen_table = 
    match queen_played, queen_table with 
    | true, false -> play_highest hand pile
    | true, true
    | false, true -> play_easy hand pile
    | false, false -> if List.length pile = 3 then play_highest hand pile 
      else play_easy hand pile


  and play_hard hand pile qspade qspadetable =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in 
    match pile_suite with 
    | Club -> play_easy hand pile 
    | Diamond -> play_easy hand pile
    | Spade -> play_med_helper hand pile qspade qspadetable
    | Heart -> play_easy hand pile

  and list_to_deck pile acc =   
    match pile with 
    | (card, _) :: t -> list_to_deck t (PartialDeck.insert card acc)
    | [] -> acc

  and play hand pile diff =
    let pile' = list_to_deck pile (PartialDeck.empty) in 
    let qs = {suite = Spade; rank = Queen} in 
    let qspadetable = (PartialDeck.mem qs pile') in 
    match diff with 
    | "easy" -> play_easy hand pile
    | "medium" -> play_med hand pile true qspadetable
    | "hard" -> play_easy hand pile 
    | _ -> play_easy hand pile

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