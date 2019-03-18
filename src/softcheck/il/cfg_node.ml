open Batteries

module Make(E : sig
    type t
    val to_string : t -> string
  end) = struct
  type expr = E.t

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

  let rec stmt_to_string =
    let open Printf in
    function
      Cfg_var_decl v -> sprintf "decl %s" (get_node_data v)
    | Cfg_assign (lv, rv) -> sprintf "%s = %s" (to_string lv)
                               (to_string rv)
    | Cfg_guard e -> sprintf "test %s" (to_string e)
    | Cfg_jump -> sprintf "jump"
    | Cfg_call (f, args) ->
        sprintf "%s (%s)" (to_string f)
          (Utils.sprint_list ~first:"" ~last:"" ~sep:", "
             to_string args)

  and to_string : type a. a node -> string = fun n ->
    let open Printf in
    let rec aux (n : a node) =
      match n.data with
        Stmt _ -> stmt_to_string (get_node_data n)
      | Decl d -> d
      | Expr e -> E.to_string e in
    aux n
end
