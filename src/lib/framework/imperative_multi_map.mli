module Make (S : Set.S) : sig
  type 'key t

  val create : int -> 'key t

  val find : 'key t -> 'key -> S.t

  val add : 'key t -> 'key -> S.elt -> unit
end
