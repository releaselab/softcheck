open Batteries

type 'a stmt =
    Cfg_var_decl of string
  | Cfg_assign of 'a * 'a
  | Cfg_if of 'a * 'a node
  | Cfg_if_else of 'a * 'a node * 'a node
  | Cfg_while of 'a * 'a node
  | Cfg_jump of int
  | Cfg_call of 'a * 'a list
  | Cfg_seq of 'a node *'a node

and 'a node = {
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

module Make_set(E : sig type t end) = struct
  include Set.Make(struct
      type t = E.t node
      let compare x y = compare x.id y.id
    end)
end

type 'a t = 'a node
