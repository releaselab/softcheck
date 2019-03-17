open Batteries

(* FIXME: check jumps *)
let rec init n =
  let open Softlang in
  match get_node_data n with
    Cfg_assign _
  | Cfg_call _
  | Cfg_if _
  | Cfg_if_else _
  | Cfg_jump _
  | Cfg_var_decl _
  | Cfg_while _ -> n
  | Cfg_seq (x, _) -> init x

let final x =
  let open Softlang in
  let open Set.Infix in
  let rec final_rec acc n = match get_node_data n with
      Cfg_assign _
    | Cfg_call _
    | Cfg_jump _
    | Cfg_var_decl _
    | Cfg_while _ -> acc <-- n
    | Cfg_if (_, x) -> final_rec acc x
    | Cfg_if_else (_, x, y) -> final_rec (final_rec acc x) y
    | Cfg_seq (_, x) -> final_rec acc x
  in final_rec Set.empty x

let rev_pair x y = (y, x)

let flow x =
  let open Softlang in
  let open Set.Infix in
  let ht = Hashtbl.create 10 in
  let rec flow_rec (nodes, flow) n =
    let new_node data =
      let data' = match data with
          Stmt s -> Cfg_node.Stmt s
        | Decl d -> Cfg_node.Decl d
        | Expr e -> Cfg_node.Expr e in
      let n' = Cfg_node.create ~loc:n.Softlang.loc data in
      Hashtbl.add ht n n' in
    match get_node_data n with
      Cfg_assign (lv, rv) ->
        let () = new_node (Stmt (Cfg_assign (lv, rv))) in
        nodes <-- n, flow
    | Cfg_var_decl v ->
        let () = new_node (Cfg_var_decl v) in
        nodes <-- n, flow
    | Cfg_call (f, args) ->
        let () = new_node (Cfg_call (f, args)) in
        nodes <-- n, flow
    | Cfg_seq (s_1, s_2) ->
        let init_s2 = init s_2 in
        let final_s1 = final s_1 in
        let flow' = flow ||. Set.map (rev_pair init_s2) final_s1 in
        flow_rec (flow_rec (nodes, flow') s_1) s_2
    | Cfg_while (e, b) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = nodes <-- n in
        let flow' = Set.map (rev_pair n)
            (final b) <-- (n, init b) ||. flow in
        flow_rec (nodes', flow') b
    | Cfg_if (e, b)          ->
        let () = new_node (Cfg_guard e) in
        let nodes' = nodes <-- n in
        let flow' = flow <-- (n, init b) in
        flow_rec (nodes', flow') b
    | Cfg_if_else (e, x, y)  ->
        let () = new_node (Cfg_guard e) in
        let nodes' = nodes <-- n in
        let flow' = flow <-- (n, init x) <-- (n, init y) in
        flow_rec (flow_rec (nodes', flow') x) y
    | Cfg_jump _ -> (nodes, flow) in
  let nodes, flow = flow_rec (Set.empty, Set.empty) x in
  let initial = Hashtbl.find ht (init x) in
  let nodes' = Set.map (Hashtbl.find ht) nodes in
  ht, Set.singleton initial, nodes', Set.map (fun (a, b) -> Hashtbl.find ht a, Hashtbl.find ht b) flow

let flowR n = let ht, _, nodes, flow = flow n in
  (* TODO: initial *)
  ht, nodes, Set.map (fun (a,b) -> (b,a)) flow
