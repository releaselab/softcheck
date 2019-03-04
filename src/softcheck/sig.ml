open Batteries

module type Ast = sig
  type stmt
  type expr
  type func
  type program
end

module type Printer = sig
  type expr
  val expr_to_string : expr -> string
end

module type Flow = sig
  type ast_stmt
  type block
  type program
  type func
  val to_string : block -> string
  val init : ast_stmt -> int
  val final : ast_stmt -> int Set.t
  val flow : func -> (int * int) Set.t
  val flowR : func -> (int * int) Set.t
end

module type Flow_graph = sig
  type t
  type vertex
  type edge_label
  type program

  val create : unit -> t
  val generate_from_program : program -> t
  val get : t -> int -> vertex
  val add : t -> string -> (int * vertex) -> unit
  val connect : t -> ?label:edge_label -> int -> int -> unit
  val inflow : t -> int -> int list
  val outflow : t -> int -> int list
  val extremal : t -> int -> unit
  val is_extremal : t -> int -> bool
  val extremalR : t -> int -> unit
  val is_extremalR : t -> int -> bool
  val get_blocks : t -> (int, vertex) Hashtbl.t
  val get_func_id : t -> int -> string
  val show : t -> unit
end

module type Inter_flow_graph = sig
  include Flow_graph

  val inter_flow : t -> (int * int * int * int) list
  val callees : t -> int -> int list
end

module type Transfer = sig
  type vertex
  type state

  val initial_state : state

  val f : string -> int -> vertex -> state -> state
end

module type Inter_transfer = sig
  include Transfer

  val f : string -> int -> vertex -> state -> state
  val f1 : string -> int -> vertex -> state -> state
  val f2 : string -> string -> int -> vertex -> state -> state -> state
end

module type Lattice = sig
  include Fix.PROPERTY

  val lub : property -> property -> property
  val to_string : property -> string
end

module type Element = sig
  type t
  val to_string: t -> string
end

module type Dependencies = sig
  type g_t

  val outdep : g_t -> int -> int list
  val indep : g_t -> int -> int list
  val is_extremal : g_t -> int -> bool
end

module type Call_context = sig
  type t

  val to_string : t -> string
  val initial_context : t
  val make_call_context : t -> int -> 'a -> int -> t
end
