open Batteries

(* FIXME: check jumps *)
let rec init n = match n.Softlang.stmt with
    Cfg_assign _
  | Cfg_call _
  | Cfg_if _
  | Cfg_if_else _
  | Cfg_jump _
  | Cfg_var_decl _
  | Cfg_while _ -> n.id
  | Cfg_seq (x, _) -> init x

let final x =
  let open Set.Infix in
  let rec final_rec acc n = match n.Softlang.stmt with
      Cfg_assign _
    | Cfg_call _
    | Cfg_jump _
    | Cfg_var_decl _
    | Cfg_while _ -> acc <-- n.id
    | Cfg_if (_, x) -> final_rec acc x
    | Cfg_if_else (_, x, y) -> final_rec (final_rec acc x) y
    | Cfg_seq (_, x) -> final_rec acc x
  in final_rec Set.empty x

let rev_pair x y = (y, x)

let flow x =
  let open Set.Infix in
  let ht = Hashtbl.create 10 in
  let rec flow_rec (nodes, flow) n =
    let new_node s =
      let n' = Cfg_node.create ~loc:n.Softlang.loc s in
      let () = Hashtbl.add ht n.id n' in
      n' in
    match n.Softlang.stmt with  
      Cfg_assign (lv, rv) ->
        let n = new_node (Cfg_assign (lv, rv)) in
        nodes <-- n, flow
    | Cfg_var_decl v ->
        let n = new_node (Cfg_var_decl v) in
        nodes <-- n, flow
    | Cfg_call (f, args) ->
        let n = new_node (Cfg_call (f, args)) in
        nodes <-- n, flow
    | Cfg_seq (s_1, s_2) ->
        let init_s2 = init s_2 in
        let final_s1 = final s_1 in
        let flow' = flow ||. Set.map (rev_pair init_s2) final_s1 in
        flow_rec (flow_rec (nodes, flow') s_1) s_2
    | Cfg_while (e, b) ->
        let n = new_node (Cfg_guard e) in
        let nodes' = nodes <-- n in
        let flow' = Set.map (rev_pair n.id)
            (final b) <-- (n.id, init b) ||. flow in
        flow_rec (nodes', flow') b
    | Cfg_if (e, b)          ->
        let n = new_node (Cfg_guard e) in
        let nodes' = nodes <-- n in
        let flow' = flow <-- (n.id, init b) in
        flow_rec (nodes', flow') b
    | Cfg_if_else (e, x, y)  ->
        let n = new_node (Cfg_guard e) in
        let nodes' = nodes <-- n in
        let flow' = flow <-- (n.id, init x) <-- (n.id, init y) in
        flow_rec (flow_rec (nodes', flow') x) y
    | Cfg_jump _ -> (nodes, flow) in
  let nodes, flow = flow_rec (Set.empty, Set.empty) x in
  nodes, Set.map (fun (a, b) -> Hashtbl.find ht a, Hashtbl.find ht b) flow

let flowR n = let nodes, flow = flow n in
  nodes, Set.map (fun (a,b) -> (b,a)) flow
