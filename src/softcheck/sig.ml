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
  type block
  type vertex
  type t = {
    correspondence: (block, vertex) Hashtbl.t; 
    nodes: vertex Set.t;
    initial: vertex Set.t;
    final: vertex Set.t;
    flow: (vertex * vertex) Set.t
  }

  val init : block -> block
  val final : block -> block Set.t
  val flow : block -> t
  val flowR : block -> t
end

module type Flow_graph = sig
  type expr
  type vertex
  type edge_label = Normal | If_true | If_false
  type program
  type t

  val create : unit -> t
  val inflow : t -> int -> int list
  val outflow : t -> int -> int list
  val is_extremal : t -> int -> bool
  val is_extremalR : t -> int -> bool
  val add : t -> string -> vertex -> unit
  val get : t -> int -> vertex
  val connect : t -> ?label:edge_label -> vertex -> vertex -> unit
  val get_blocks : t -> (int, vertex) Hashtbl.t
  val get_func_id : t -> int -> string
  val extremal : t -> int -> unit
  val extremalR : t -> int -> unit
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

  val f : string -> vertex -> state -> state
end

module type Inter_transfer = sig
  include Transfer

  val f : string -> vertex -> state -> state
  val f1 : string -> vertex -> state -> state
  val f2 : string -> string -> vertex -> state -> state -> state
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

module type Call_context = sig
  type t
  type vertex

  val to_string : t -> string
  val initial_context : t
  val make_call_context : t -> int -> 'a -> int -> t
end
