module type S = sig
  type expr

  type ident = string

  type stmt =
      Cfg_var_decl of ident t
    | Cfg_assign of expr t * expr t
    | Cfg_guard of expr t
    | Cfg_jump
    | Cfg_call of expr t * expr t list

  and _ node_data =
      Stmt : stmt -> stmt node_data
    | Decl : ident -> ident node_data
    | Expr : expr -> expr node_data

  and 'a t = {
    id: int;
    loc: Common.loc;
    data: 'a node_data
  }

  val create : ?loc : Common.loc -> 'a node_data -> 'a t

  val get_node_data : 'a t -> 'a

  val to_string : 'a t -> string
end
