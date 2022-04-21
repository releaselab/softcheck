open! Core
open Scil

module type FlowGraph = sig
  type block

  module Vertex = Label

  type edge_label = Normal | If_true | If_false
  type program
  type t

  val create : unit -> t
  val inflow : t -> Vertex.t -> Vertex.t list
  val outflow : t -> Vertex.t -> Vertex.t list
  val is_extremal : t -> Vertex.t -> bool
  val is_extremalR : t -> Vertex.t -> bool
  val add : t -> string -> block -> unit
  val get : t -> Vertex.t -> block
  val connect : t -> ?label:edge_label -> Vertex.t -> Vertex.t -> unit
  val get_blocks : t -> (Vertex.t, block) Hashtbl.t
  val get_func_id : t -> Vertex.t -> string
  val extremal : t -> Vertex.t -> unit
  val extremalR : t -> Vertex.t -> unit
  val labels : t -> Set.M(Label).t
  val dot_output : t -> string -> unit
  val display_with_gv : t -> unit
  val show : t -> unit
  val generate_from_program : program -> t
end

module type InterFlowGraph = sig
  include FlowGraph

  val inter_flow : t -> (Vertex.t * Vertex.t * Vertex.t * Vertex.t) list
  (* TODO: val callees : t -> int -> int list *)
end

module Make_cfg : functor
  (E : Expr.S)
  (S : Softlang.S with type expr = E.t)
  (C : Cfg_node.S with type expr = E.t)
  (F : Flow.S
         with type expr = E.t
          and type program = S.program
          and module Ast_block = S.Stmt
          and module Cfg_block = C)
  (P : sig
     type t

     val convert_program_to_sl : t -> S.program
   end)
  -> FlowGraph with type block = C.t and type program = P.t

module Make_inter_cfg : functor
  (E : Expr.S)
  (S : Softlang.S with type expr = E.t)
  (C : Cfg_node.S with type expr = E.t)
  (F : Flow.S
         with type expr = E.t
          and type program = S.program
          and module Ast_block = S.Stmt
          and module Cfg_block = C)
  (P : sig
     type t

     val convert_program_to_sl : t -> S.program
   end)
  -> InterFlowGraph with type block = C.t and type program = P.t
