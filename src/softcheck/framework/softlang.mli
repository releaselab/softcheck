open Batteries

type decl = string

type 'a stmt =
    Cfg_var_decl of decl
  | Cfg_assign of 'a * 'a
  | Cfg_if of 'a * 'a node
  | Cfg_if_else of 'a * 'a node * 'a node
  | Cfg_while of 'a * 'a node
  | Cfg_jump of int
  | Cfg_call of 'a * 'a list
  | Cfg_seq of 'a node * 'a node

and 'a node = { id : int; loc : Common.loc; stmt : 'a stmt; }

val create : ?loc:Common.loc -> 'a stmt -> 'a node

module Make_set : functor (E : sig type t end) -> Set.S

type 'a t = 'a node

type 'a func = string * decl list * 'a t

type 'a program = decl list * 'a func list
