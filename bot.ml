open Partialdeck
open Card

module type BotSig = sig 

  val play : PartialDeck.t -> (card * int) list -> card
  val lead : PartialDeck.t -> (card * int) list -> card
  val pass : PartialDeck.t -> (card * int) list -> card list

end

module Bot:BotSig = struct

  let play hand pile =
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