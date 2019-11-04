module type QueueSig = sig

  type 'a t

  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
  val peek : 'a t -> 'a
  val is_empty : 'a t -> bool
  val size : 'a t -> int

end

module ListQueue:QueueSig = struct

  type 'a t = {
    in_list : 'a list;
    out_list : 'a list;
  }

  let empty = 
    {
      in_list = [];
      out_list = [];
    }

  let push v t = 
    {
      t with 
      in_list = v::t.in_list;
    }

  let rev t = 
    if List.length t.out_list = 0 then 
      {
        out_list = List.rev t.in_list;
        in_list = [];
      } 
    else t

  let pop t =
    let t' = rev t in
    {
      t' with 
      out_list = List.tl t'.out_list;
    }

  let peek t = 
    List.hd ((rev t).out_list)

  let is_empty t =
    List.length t.in_list = 0 && List.length t.out_list = 0

  let size t =
    List.length t.in_list + List.length t.out_list

end 