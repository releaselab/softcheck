open Batteries

module Make(Sl : Softlang.S)(N : Cfg_node.S with type expr = Sl.expr) = struct
  (* FIXME: check jumps *)
  type block = Sl.stmt Sl.t
  type vertex = N.stmt N.t

  type t = {
    correspondence: (block, vertex) Hashtbl.t; 
    nodes: vertex Set.t;
    initial: vertex Set.t;
    final: vertex Set.t;
    flow: (vertex * vertex) Set.t
  }

  let rec init n =
    let open Sl in
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
    let open Sl in
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
    let open Sl in
    let open Set.Infix in
    let ht = Hashtbl.create 10 in
    let rec flow_rec (nodes, flow) n =
      let new_expr_node node =
        let open N in
        create ~loc:node.Sl.loc (Expr (Sl.get_node_data node)) in
      let new_decl_node node =
        let open N in
        create ~loc:node.Sl.loc (Decl (Sl.get_node_data node)) in
      let new_node data =
        let n' = N.create ~loc:n.Sl.loc data in
        Hashtbl.add ht n n' in
      match get_node_data n with
        Cfg_assign (lv, rv) ->
          let lv' = new_expr_node lv in
          let rv' = new_expr_node rv in
          let () = new_node (Stmt (Cfg_assign (lv', rv'))) in
          nodes <-- n, flow
      | Cfg_var_decl v ->
          let v' = new_decl_node v in
          let () = new_node (Stmt (Cfg_var_decl v')) in
          nodes <-- n, flow
      | Cfg_call (f, args) ->
          let f' = new_expr_node f in
          let args' = List.map new_expr_node args in
          let () = new_node (Stmt (Cfg_call (f', args'))) in
          nodes <-- n, flow
      | Cfg_seq (s_1, s_2) ->
          let init_s2 = init s_2 in
          let final_s1 = final s_1 in
          let flow' = flow ||. Set.map (rev_pair init_s2) final_s1 in
          flow_rec (flow_rec (nodes, flow') s_1) s_2
      | Cfg_while (e, b) ->
          let e' = new_expr_node e in
          let () = new_node (Stmt (Cfg_guard e')) in
          let nodes' = nodes <-- n in
          let flow' = Set.map (rev_pair n)
              (final b) <-- (n, init b) ||. flow in
          flow_rec (nodes', flow') b
      | Cfg_if (e, b)          ->
          let e' = new_expr_node e in
          let () = new_node (Stmt (Cfg_guard e')) in
          let nodes' = nodes <-- n in
          let flow' = flow <-- (n, init b) in
          flow_rec (nodes', flow') b
      | Cfg_if_else (e, x, y)  ->
          let e' = new_expr_node e in
          let () = new_node (Stmt (Cfg_guard e')) in
          let nodes' = nodes <-- n in
          let flow' = flow <-- (n, init x) <-- (n, init y) in
          flow_rec (flow_rec (nodes', flow') x) y
      | Cfg_jump _ -> (nodes, flow) in
    let nodes', flow = flow_rec (Set.empty, Set.empty) x in
    let initial = Hashtbl.find ht (init x) |> Set.singleton in
    let final = Set.map (Hashtbl.find ht) (final x) in
    let nodes = Set.map (Hashtbl.find ht) nodes' in
    let flow = Set.map (fun (a, b) -> Hashtbl.find ht a, Hashtbl.find ht b) flow in
    { correspondence = ht; initial; final; nodes; flow }

  let flowR n =
    let f = flow n in
    let flow = Set.map (fun (a,b) -> (b,a)) f.flow in
    { f with initial = f.final; final = f.initial; flow }
end
