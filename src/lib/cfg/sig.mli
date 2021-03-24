open Scil

module type Flow = sig
  module Ast_block : Utils.Collections.WithCollections

  type ast_block = Ast_block.t

  module Cfg_block : Utils.Collections.WithCollections

  type cfg_block = Cfg_block.t

  module Edge :
    Utils.Collections.WithCollections with type t = ast_block * ast_block

  type edge = Edge.t

  type t = {
    correspondence : (ast_block, cfg_block) Hashtbl.t;
    blocks : Ast_block.Set.t;
    initial : Ast_block.Set.t;
    final : Ast_block.Set.t;
    flow : Edge.Set.t;
  }

  val init : ast_block -> ast_block

  val final : ast_block -> Ast_block.Set.t

  val flow : ast_block -> t

  val flowR : ast_block -> t
end

module type FlowGraph = sig
  type expr

  type block

  type vertex = Label.t

  type edge_label = Normal | If_true | If_false

  type program

  type t

  val create : unit -> t

  val inflow : t -> vertex -> vertex list

  val outflow : t -> vertex -> vertex list

  val is_extremal : t -> vertex -> bool

  val is_extremalR : t -> vertex -> bool

  val add : t -> string -> block -> unit

  val get : t -> vertex -> block

  val connect : t -> ?label:edge_label -> vertex -> vertex -> unit

  val get_blocks : t -> (vertex, block) Hashtbl.t

  val get_func_id : t -> vertex -> string

  val extremal : t -> vertex -> unit

  val extremalR : t -> vertex -> unit

  val labels : t -> Label.Set.t

  val dot_output : t -> string -> unit

  val display_with_gv : t -> unit

  val show : t -> unit

  val generate_from_program : program -> t
end

module type InterFlowGraph = sig
  include FlowGraph

  val inter_flow : t -> (vertex * vertex * vertex * vertex) list
  (* TODO: val callees : t -> int -> int list *)
end
