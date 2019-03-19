module Make : functor
  (E : sig
     type t
     val to_string : t -> string
   end) -> Node_sig.S with type expr = E.t
