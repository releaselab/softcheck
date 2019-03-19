open Batteries

module Make(E : sig type t end) = struct
  type expr = E.t

  type ident = string

  type stmt =
      Cfg_var_decl of ident t
    | Cfg_assign of expr t * expr t
    | Cfg_if of expr t * stmt t
    | Cfg_if_else of expr t * stmt t * stmt t
    | Cfg_while of expr t * stmt t
    | Cfg_jump of stmt t
    | Cfg_call of expr t * expr t list
    | Cfg_seq of stmt t * stmt t

  and _ node_data =
      Stmt : stmt -> stmt node_data
    | Decl : ident -> ident node_data
    | Expr : expr -> expr node_data

  and 'a t = {
    id: int;
    loc: Common.loc;
    data: 'a node_data
  }

  let get_node_data : type a. a t -> a = fun n -> match n.data with
      Stmt s -> s
    | Decl d -> d
    | Expr e -> e

  let counter = ref (-1)

  let next_counter () =
    let () = counter := !counter + 1 in
    !counter

  let create ?loc:(loc=Common.Unknown) data =
    { id = next_counter (); loc; data }

  type func = string * ident t list * stmt t

  type program = ident t list * func list
end

module type S = sig
  type expr

  type ident = string

  type stmt =
      Cfg_var_decl of ident t
    | Cfg_assign of expr t * expr t
    | Cfg_if of expr t * stmt t
    | Cfg_if_else of expr t * stmt t * stmt t
    | Cfg_while of expr t * stmt t
    | Cfg_jump of stmt t
    | Cfg_call of expr t * expr t list
    | Cfg_seq of stmt t * stmt t

  and _ node_data =
      Stmt : stmt -> stmt node_data
    | Decl : ident -> ident node_data
    | Expr : expr -> expr node_data

  and 'a t = {
    id: int;
    loc: Common.loc;
    data: 'a node_data
  }

  val get_node_data : 'a t -> 'a

  val create : ?loc:Common.loc -> 'a node_data -> 'a t

  type func = string * ident t list * stmt t

  type program = ident t list * func list
end
