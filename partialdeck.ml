open Card

module type PartialDeckSig = sig

  type t

  val empty : t 
  val full : t
  val insert : card -> t -> t 
  val remove : card -> t -> t
  val move : card -> t -> t -> (t * t)
  val move_at_index : int -> t -> t -> (t * t)
  val mem : card -> t -> bool
  val size: t -> int
  val is_empty: t -> bool
  val random_card: t -> card option
  val to_list: t -> (card * int) list
  val find: int -> t -> card option
  val voided: suite -> t -> bool
  val count_points: t -> int
  val merge: t -> t -> t
  val contains_hearts: t -> bool
  val string_of_partialdeck: t -> string
  val lowest: t -> suite -> card
  val highest: t -> suite -> card
  val remove_cards: card list -> t -> t
  val add_cards: card list -> t -> t
  val shoot_the_moon: t -> bool
  val only_bad : t -> bool 
end


module PartialDeck:PartialDeckSig = struct


  type t = card list

  let mem card t = 
    List.mem card t 

  let insert card (t:t) = 
    if mem card t then raise DuplicateCard 
    else (card :: t) |> List.sort compare

  let empty = []

  let full = 
    let rec aux rank suite deck = 
      if rank > 1 then
        aux (rank - 1) suite (insert {suite=suite; rank=int_to_rank rank} deck)
      else deck
    in
    aux 14 Spade empty |> aux 14 Heart |> aux 14 Diamond |> aux 14 Club

  let to_list t =
    let rec aux t n =
      match t with 
      | h::t -> (h, n)::(aux t (n+1))
      | [] -> []
    in
    aux t 1

  let size t =
    List.length t

  let is_empty t = 
    if size t = 0 then true else false

  let remove card t = 
    if List.mem card t then List.filter (fun x -> x <> card) t
    else raise CardNotFound

  let move card t1 t2 =
    (remove card t1, insert card t2)

  let move_at_index i t1 t2 =
    match List.nth_opt (to_list t1) (i-1) with
    | Some (card, index) -> (remove card t1, insert card t2)
    | None -> raise CardNotFound

  let random_card t =
    Random.self_init ();
    if is_empty t then None else Some (List.nth t (Random.int (size t)))

  let rec find n t = 
    match t with
    | [] -> None
    | h :: t -> if n = 1 then Some h else find (n-1) t

  let voided s t =
    match List.filter (fun c -> c.suite = s) t |> List.length with
    | 0 -> true
    | _ -> false

  let count_points t =
    List.fold_left 
      (fun acc card -> 
         match card with 
         | {suite=Heart} -> acc + 1
         | {suite=Spade;rank=Queen} -> acc + 13
         | {suite=Diamond; rank=Ten} -> acc - 10
         | _ -> acc
      )
      0 t

  let merge t1 t2 =
    List.fold_left (fun acc c -> insert c acc) t2 t1

  let contains_hearts t = 
    (t |> List.filter (fun c -> c.suite = Heart) |> List.length) <> 0

  let string_of_partialdeck p = 
    List.fold_left 
      (fun acc (c,i) -> 
         acc ^ "(" ^ (suite_to_string c.suite) ^ 
         (rank_to_string c.rank) ^ "," ^ (string_of_int i) ^ ")" ^ "; "
      )
      ""
      (to_list p)

  let lowest t s =
    match (List.filter (fun card -> card.suite = s) t) with
    | h::t -> h
    | [] -> raise CardNotFound

  let highest t s =
    match List.rev (List.filter (fun card -> card.suite = s) t) with
    | h::t -> h
    | [] -> raise CardNotFound

  let rec remove_cards card_l st = 
    match card_l with 
    | h::t -> remove_cards t (remove h st)
    | [] -> st

  let rec add_cards card_l st = 
    match card_l with 
    | h::t -> add_cards t (insert h st)
    | [] -> st

  let shoot_the_moon t =
    let cards = 
      [
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
    in
    List.for_all (fun card -> mem card t) cards

  let only_bad t =
    let bad_cards = 
      [
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
    in  
    List.for_all (fun card -> mem card bad_cards) t

end