open Partialdeck
open Card
open Round

module type BotSig = sig 

  val play : Round.t -> int -> card
  val lead : Round.t -> int -> card

end

module Bot = struct

  let play state id = 
    let hand = Round.bot_hand state id in
    let pile = Round.bot_pile state in
    let pile_suite = (List.nth pile ((List.length pile) - 1) |> fst).suite in
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


  let lead state id = 
    let hand = Round.bot_hand state id in
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