module type Ast = sig
  type stmt

  type expr

  type func

  type program
end

module type CallContext = sig
  type t

  type vertex

  val to_string : t -> string

  val initial_context : t

  val make_call_context : t -> int -> 'a -> int -> t
end

module type Transfer = sig
  type vertex

  type state

  val initial_state : state

  val f : string -> vertex -> state -> state
end

module type InterTransfer = sig
  include Transfer

  val f : string -> vertex -> state -> state

  val f1 : string -> vertex -> state -> state

  val f2 : string -> string -> vertex -> state -> state -> state
end
