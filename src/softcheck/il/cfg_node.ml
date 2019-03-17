open Batteries

type ident = string

type 'a stmt =
    Cfg_var_decl of ident node
  | Cfg_assign of 'a node * 'a node
  | Cfg_guard of 'a node
  | Cfg_jump
  | Cfg_call of 'a node * 'a node list

and _ node_data =
    Stmt : 'a stmt -> ('a stmt) node_data
  | Decl : ident -> ident node_data
  | Expr : 'a -> 'a node_data

and 'a node = {
  id: int;
  loc: Common.loc;
  data: 'a node_data
}

let counter = ref (-1)

let next_counter () =
  let () = counter := !counter + 1 in
  !counter

let create ?loc:(loc=Common.Unknown) data =
  { id = next_counter (); loc; data }

let get_node_data : type a. a node -> a = fun n -> match n.data with
    Stmt s -> s
  | Decl d -> d
  | Expr e -> e

let to_string expr_to_string =
  let open Printf in
  function
    Cfg_var_decl v -> sprintf "decl %s" (get_node_data v)
  | Cfg_assign (lv, rv) -> sprintf "%s = %s" (expr_to_string lv) (expr_to_string rv)
  | Cfg_guard e -> sprintf "test %s" (expr_to_string e)
  | Cfg_jump -> sprintf "jump"
  | Cfg_call (f, args) ->
      sprintf "%s (%s)" (expr_to_string f)
        (Utils.sprint_list ~first:"" ~last:"" ~sep:", " expr_to_string args)

module Make_set(E : sig type t end) = struct
  include Set.Make(struct
      type t = E.t node
      let compare x y = compare x.id y.id
    end)
end

type 'a t = 'a node
