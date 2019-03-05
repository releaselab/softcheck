open Batteries

open Cfg_node.Intermediate

module Node = Cfg_node.Processed

(* FIXME: check jumps *)

let rec init n = match n.stmt with
    Cfg_assign _
  | Cfg_call _
  | Cfg_if _
  | Cfg_if_else _
  | Cfg_jump _
  | Cfg_skip
  | Cfg_var_decl _
  | Cfg_while _ -> n.id
  | Cfg_seq (x, _) -> init x

let final =
  let open Set.Infix in
  let rec final_rec acc n = match n.stmt with
      Cfg_assign _
    | Cfg_call _
    | Cfg_skip
    | Cfg_jump _
    | Cfg_var_decl _
    | Cfg_while _ -> acc <-- n.id
    | Cfg_if (_, x) -> final_rec acc x
    | Cfg_if_else (_, x, y) -> final_rec (final_rec acc x) y
    | Cfg_seq (_, x) -> final_rec acc x
  in final_rec Set.empty

let rev_pair x y = (y, x)

let flow =
  let while_counter = ref (-1) in
  let next_counter () = while_counter := !while_counter + 1; ref in
  let open Set.Infix in
  let rec flow_rec (nodes, flow) n = match n.stmt with
      Cfg_assign (lv, rv) ->
        let n = Node.(create ~loc:n.loc (Cfg_assign (lv, rv))) in
        nodes <-- n, flow
    | Cfg_skip -> nodes, flow
    | Cfg_var_decl v ->
        let n = Node.(create ~loc:n.loc (Cfg_var_decl v)) in
        nodes <-- n, flow
    | Cfg_call (f, args) ->
        let n = Node.(create ~loc:n.loc (Cfg_call (f, args))) in
        nodes <-- n, flow
    | Cfg_seq (s_1, s_2) ->
        let init_s2 = init s_2 in
        let final_s1 = final s_1 in
        let flow' = flow ||. Set.map (rev_pair init_s2) final_s1 in
        flow_rec (flow_rec (nodes, flow') s_1) s_2
    | Cfg_while (e, b) ->
        let n = Node.(create ~loc:n.loc (Cfg_jump ("_while" ^ string_of_int (next_counter ())))) in
        let flow' = Set.map (rev_pair n.id) (final b) <-- (n.id, init b) ||. flow in

        flow_rec () x
    | Cfg_if (_, x)          -> flow_rec (acc <-- (n.id, init x)) x
    | Cfg_if_else (_, x, y)  -> flow_rec (flow_rec (acc <-- (n.id, init x) <-- (n.id, init y)) x) y
    | Cfg_jump -> acc
  in flow_rec Set.empty

let flowR = Set.map (fun (a,b) -> (b,a)) % flow
