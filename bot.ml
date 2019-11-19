open Partialdeck
open Card
open Round

module type BotSig = sig 

  val play : PartialDeck.t -> (card * int) list -> card
  val lead : PartialDeck.t -> (card * int) list -> card
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

  and play_med_helper hand pile queen_played queen_table heart_pile = 
    match queen_played, queen_table, heart_pile with 
    | true, false, _ -> play_highest hand pile
    | true, true, _
    | false, true, _ -> play hand pile
    | false, false, _ -> if List.length pile = 3 then play_highest hand pile 
      else play hand pile


  and play_med hand pile qspade qspadetable heart_pile =
    let pile_suite = (fst (pile |> List.rev |> List.hd)).suite in 
    match pile_suite with 
    | Club -> play hand pile 
    | Diamond -> play hand pile
    | Spade -> play_med_helper hand pile qspade qspadetable heart_pile
    | Heart -> play hand pile


  and play hand pile =
    play_easy hand pile

  let lead hand pile = 
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

  let pass deck pile = 
    failwith "uni"

end