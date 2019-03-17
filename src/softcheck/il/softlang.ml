open Batteries

type decl = string

type 'a stmt =
    Cfg_var_decl of decl node
  | Cfg_assign of 'a node * 'a node
  | Cfg_if of 'a node * 'a stmt node
  | Cfg_if_else of 'a node * 'a stmt node * 'a stmt node
  | Cfg_while of 'a node * 'a stmt node
  | Cfg_jump of 'a stmt node
  | Cfg_call of 'a node * 'a node list
  | Cfg_seq of 'a stmt node *'a stmt node

and _ node_data =
    Stmt : 'a stmt -> 'a stmt node_data
  | Decl : decl -> decl node_data
  | Expr : 'a -> 'a node_data

and 'a node = {
  id: int;
  loc: Common.loc;
  data: 'a node_data
}

let get_node_data : type a. a node -> a = fun n -> match n.data with
    Stmt s -> s
  | Decl d -> d
  | Expr e -> e

let counter = ref (-1)

let next_counter () =
  let () = counter := !counter + 1 in
  !counter

let create ?loc:(loc=Common.Unknown) data =
  { id = next_counter (); loc; data }

module Make_set(E : sig type t end) = struct
  include Set.Make(struct
      type t = E.t node
      let compare x y = compare x.id y.id
    end)
end

type 'a t = 'a node

type 'a func = string * decl list * 'a t

type 'a program = decl list * ('a func) list
