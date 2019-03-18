open Batteries

module type NODE = sig
  type expr

  type ident = string

  type stmt =
      Cfg_var_decl of ident node
    | Cfg_assign of expr node * expr node
    | Cfg_guard of expr node
    | Cfg_jump
    | Cfg_call of expr node * expr node list

  and _ node_data =
      Stmt : stmt -> stmt node_data
    | Decl : ident -> ident node_data
    | Expr : expr -> expr node_data

  and 'a node = {
    id: int;
    loc: Common.loc;
    data: 'a node_data
  }

  type 'a t = 'a node

  val create : ?loc : Common.loc -> 'a node_data -> 'a t

  val get_node_data : 'a t -> 'a

  val to_string : 'a t -> string
end

module Make : functor
  (E : sig
     type t
     val to_string : t -> string
   end) -> NODE with type expr = E.t
