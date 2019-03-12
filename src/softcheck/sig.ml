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
  type expr
  type vertex = expr Cfg_node.t
  type edge_label = Normal | If_true | If_false
  type program 
  type t

  val create : unit -> t
  val inflow : t -> vertex -> vertex list
  val outflow : t -> vertex -> vertex list
  val is_extremal : t -> vertex -> bool
  val is_extremalR : t -> vertex -> bool
  val add : t -> string -> vertex -> unit
  val connect : t -> ?label:edge_label -> vertex -> vertex -> unit
  val get_blocks : t -> vertex Set.t
  val get_func_id : t -> vertex -> string
  val extremal : t -> vertex -> unit
  val extremalR : t -> vertex -> unit
  val dot_output : t -> string -> unit
  val display_with_gv : t -> unit
  val show : t -> unit
  val generate_from_program : program -> t
end

module type Inter_flow_graph = sig
  include Flow_graph

  val inter_flow : t -> (vertex * vertex * vertex * vertex) list
  (* TODO: val callees : t -> int -> int list *)
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
  type vertex

  val outdep : g_t -> vertex -> vertex list
  val indep : g_t -> int -> int list
  val is_extremal : g_t -> int -> bool
end

module type Call_context = sig
  type t

  val to_string : t -> string
  val initial_context : t
  val make_call_context : t -> int -> 'a -> int -> t
end
