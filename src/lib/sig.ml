open Batteries

module type Ast = sig
  type stmt
  type expr
  type label = int
  type func
  type program
  type ident = string
end

module type Printer = sig
  type expr
  val exp_to_string : expr -> string
end

module type Flow = sig
  type ast_stmt
  type block
  type program
  type func
  type label = int
  val to_string : block -> string
  val init : ast_stmt -> label
  val final : ast_stmt -> label Set.t
  val blocks : func -> (label, block) Hashtbl.t
  val labels : func -> label Set.t
  val flow : func -> (label * label) Set.t
  val flowR : func -> (label * label) Set.t
end

module type Flow_graph = sig
  type t
  type program
  type stmt_label = int
  type ident = string
  type vertex
  type edge_label

  val create : unit -> t
  val generate_from_program : program -> t
  val get : t -> stmt_label -> vertex
  val add : t -> string -> (stmt_label * vertex) -> unit
  val connect : t -> ?label:edge_label -> stmt_label -> stmt_label -> unit
  val inflow : t -> stmt_label -> stmt_label list
  val outflow : t -> stmt_label -> stmt_label list
  val extremal : t -> stmt_label -> unit
  val is_extremal : t -> stmt_label -> bool
  val extremalR : t -> stmt_label -> unit
  val is_extremalR : t -> stmt_label -> bool
  val get_blocks : t -> (stmt_label, vertex) Hashtbl.t
  val get_func_id : t -> stmt_label -> ident
  val show : t -> unit
end

module type Inter_flow_graph = sig
  include Flow_graph

  val inter_flow : t -> (stmt_label * stmt_label * stmt_label * stmt_label) list
  val callees : t -> stmt_label -> stmt_label list
end

module type Transfer = sig
  type vertex
  type state
  type label = int
  type ident = string

  val initial_state : state

  val f : ident -> label -> vertex -> state -> state
end

module type Inter_transfer = sig
  include Transfer

  val f : ident -> label -> vertex -> state -> state
  val f1 : ident -> label -> vertex -> state -> state
  val f2 : ident -> ident -> label -> vertex -> state -> state -> state
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
  type label = int

  val outdep : g_t -> label -> label list
  val indep : g_t -> label -> label list
  val is_extremal : g_t -> label -> bool
end

module type Call_context = sig
  type t
  type label = int

  val to_string : t -> string
  val initial_context : t
  val make_call_context : t -> label -> 'a -> label -> t
end
