open Batteries

type 'a stmt =
    Cfg_var_decl of string
  | Cfg_assign of 'a * 'a
  | Cfg_guard of 'a
  | Cfg_jump of int
  | Cfg_call of 'a * 'a list

type 'a node = {
  id: int;
  loc: Common.loc;
  stmt: 'a stmt
}

let counter = ref (-1)

let next_counter () =
  let () = counter := !counter + 1 in
  !counter

let create ?loc:(loc=Common.Unknown) stmt =
  { id = next_counter (); loc; stmt; }

let to_string expr_to_string =
  let open Printf in
  function
    Cfg_var_decl v -> sprintf "decl %s" v
  | Cfg_assign (lv, rv) -> sprintf "%s = %s" (expr_to_string lv) (expr_to_string rv)
  | Cfg_guard e -> sprintf "test %s" (expr_to_string e)
  | Cfg_jump label -> sprintf "jump %d" label
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
