
module type QueueSig = sig

  type 'a t

  (** [empty] is an empty Queue. *)
  val empty : 'a t

  (** [push a t] is queue [t] with [a] added. *)
  val push : 'a -> 'a t -> 'a t

  (** [pop t] is queue [t] with the last element removed. *)
  val pop : 'a t -> 'a t

  (** [peek t] is the last element in [t]. *)
  val peek : 'a t -> 'a

  (** [is_empty t] is true if [t] is empty, otherwise false. *)
  val is_empty : 'a t -> bool

  (** [size t] is the number of elements in [t]. *)
  val size : 'a t -> int

end 


module ListQueue:QueueSig