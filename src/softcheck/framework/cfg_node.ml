type loc = Unknown | Pos of int * int

type var = {
  var_id: int;
  var_name: string;
}

module Processed = struct
  type 'a stmt =
      Cfg_var_decl of string
    | Cfg_assign of 'a * 'a
    | Cfg_guard of 'a
    | Cfg_jump of string
    | Cfg_call of 'a * 'a list

  type 'a t = {
    id: int;
    loc: loc;
    stmt: 'a stmt
  }

  let counter = ref (-1)

  let next_counter () =
    let () = counter := !counter + 1 in
    !counter

  let create ?loc:(loc=Unknown) stmt =
    { id = next_counter (); loc; stmt; }

  let to_string expr_to_string =
    let open Printf in
    function
      Cfg_var_decl v -> sprintf "decl %s" v
    | Cfg_assign (lv, rv) -> sprintf "%s = %s" (expr_to_string lv) (expr_to_string rv)
    | Cfg_guard e -> sprintf "test %s" (expr_to_string e)
    | Cfg_jump label -> sprintf "jump \"%s\"" label
    | Cfg_call (f, args) ->
        sprintf "%s (%s)" (expr_to_string f)
          (Utils.sprint_list ~first:"" ~last:"" ~sep:", " expr_to_string args)
end

module Intermediate = struct
  type 'a stmt =
      Cfg_var_decl of string
    | Cfg_assign of 'a * 'a
    | Cfg_if of 'a * 'a t
    | Cfg_if_else of 'a * 'a t * 'a t
    | Cfg_while of 'a * 'a t
    | Cfg_jump of string
    | Cfg_call of 'a * 'a list
    | Cfg_seq of 'a t *'a t
    | Cfg_skip

  and 'a t = {
    id: int;
    loc: loc;
    stmt: 'a stmt
  }

  let counter = ref (-1)

  let next_counter () =
    let () = counter := !counter + 1 in
    !counter

  let create ?loc:(loc=Unknown) stmt =
    { id = next_counter (); loc; stmt; }
end
