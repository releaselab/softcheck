module type VAR = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type TOKEN = sig
  type t

  val n : int

  val to_string : t -> string
end

module Make (V : VAR) (T : TOKEN) = struct
  type token = T.t

  type var = V.t

  let token_to_string = T.to_string

  let var_to_string = V.to_string

  type constant = token * var

  type subset = var * var

  type constr =
    | Constant of constant
    | Subset of subset
    | Conditional of constant * subset

  let token_id = ref (-1)

  let next_token_id () =
    let () = token_id := !token_id + 1 in
    !token_id

  let tokens_ids = Hashtbl.create T.n

  let get_token_int t =
    try Hashtbl.find tokens_ids t
    with Not_found ->
      let id = next_token_id () in
      let () = Hashtbl.add tokens_ids t id in
      id

  module VarSet = Set.Make (V)

  module Node = struct
    type t = {
      mutable vars : VarSet.t;
      conditionals : (int, subset list) Hashtbl.t;
      token_sol : Bitv.t;
    }

    let create v =
      {
        vars = VarSet.singleton v;
        conditionals = Hashtbl.create 42;
        token_sol = Bitv.create T.n false;
      }
  end

  module G = struct
    module NGraph = Graph.Imperative.Digraph.Abstract (struct
      type t = Node.t
    end)

    module D =
      Graph.Path.Dijkstra
        (NGraph)
        (struct
          type edge = NGraph.E.t

          type t = unit

          let weight _ = ()

          let compare = compare

          let add _ _ = ()

          let zero = ()
        end)

    let graph = NGraph.create ()

    let succ = NGraph.succ graph

    let edge_path_to_vertex_path el =
      let rec aux acc l =
        match (l, acc) with
        | [], _ -> acc
        | e :: t, [] -> aux [ NGraph.E.src e; NGraph.E.dst e ] t
        | e :: t, _ -> aux (acc @ [ NGraph.E.dst e ]) t
      in
      aux [] el

    let path f t =
      try
        let edges, _ = D.shortest_path graph f t in
        edge_path_to_vertex_path edges
      with Not_found -> []

    let add_vertex = NGraph.add_vertex graph

    let add_edge = NGraph.add_edge graph

    let create_vertex v = NGraph.V.create (Node.create v)

    let label = NGraph.V.label
  end

  let vars_vertex = Hashtbl.create 42

  let get_vertex v = Hashtbl.find vars_vertex v

  let get_or_put_vertex v =
    try get_vertex v
    with Not_found ->
      let n = G.create_vertex v in
      let () = Hashtbl.add vars_vertex v n in
      let () = G.add_vertex n in
      n

  let set_vertex = Hashtbl.replace vars_vertex

  let unite s t = Bitv.iteri (fun i t -> Bitv.set s i t) (Bitv.bw_or s t)

  let diff s t = Bitv.bw_and s t |> Bitv.bw_not

  let collapse_cycle = function
    | [] -> ()
    | first :: t ->
      List.iter
        (fun old ->
          let () = List.iter (fun n -> G.add_edge first n) (G.succ old) in
          let old_v = G.label old in
          let first_v = G.label first in
          let () =
            Hashtbl.iter
              (fun k v -> Hashtbl.add first_v.conditionals k v)
              old_v.conditionals
          in
          let () = unite first_v.token_sol old_v.token_sol in
          VarSet.iter
            (fun v ->
              let () = set_vertex v first in
              first_v.vars <- VarSet.add v first_v.vars)
            old_v.vars)
        t

  let rec add_and_propagate_bits s v =
    let n = G.label v in
    let old = Bitv.copy n.token_sol in
    let new_tokens = Bitv.bw_or old s in
    if new_tokens <> old then
      let () = unite n.token_sol s in
      let diff = diff new_tokens old in
      let () =
        Bitv.iteri
          (fun i _ ->
            let conditionals =
              match Hashtbl.find_opt n.conditionals i with
              | None -> []
              | Some x -> x
            in
            List.iter
              (fun (v1, v2) -> add_constraint (Subset (v1, v2)))
              conditionals)
          diff
      in
      let () = Bitv.iteri (fun i _ -> Hashtbl.remove n.conditionals i) diff in
      List.iter (add_and_propagate_bits new_tokens) (G.succ v)

  and add_constraint = function
    | Constant (t, x) ->
      let bs = Bitv.create T.n false in
      let () = Bitv.set bs (get_token_int t) true in
      add_and_propagate_bits bs (get_or_put_vertex x)
    | Subset (x, y) ->
      let v_x = get_or_put_vertex x in
      let v_y = get_or_put_vertex y in
      if v_x <> v_y then
        let () = G.add_edge v_x v_y in
        let () = add_and_propagate_bits (G.label v_x).token_sol v_y in
        collapse_cycle (G.path v_y v_x)
    | Conditional ((t, x), (y, z)) ->
      let n_x = G.label (get_or_put_vertex x) in
      if Bitv.get n_x.token_sol (get_token_int t) then
        add_constraint (Subset (y, z))
      else if y <> z then
        let conds =
          let h = n_x.conditionals in
          let k = get_token_int t in

          match Hashtbl.find_opt h k with
          | None ->
            let v = [] in
            let () = Hashtbl.add h k v in
            v
          | Some x -> x
        in
        Hashtbl.replace n_x.conditionals (get_token_int t) ((y, z) :: conds)

  let get_solution () =
    let id_to_token = Hashtbl.create (Hashtbl.length tokens_ids) in
    let () = Hashtbl.iter (fun k v -> Hashtbl.add id_to_token v k) tokens_ids in
    let sol = Hashtbl.create (Hashtbl.length vars_vertex) in
    let () =
      Hashtbl.iter
        (fun v _ ->
          Hashtbl.add sol v
            (Bitv.foldi_left
               (fun acc i t ->
                 if t then Hashtbl.find id_to_token i :: acc else acc)
               [] (get_or_put_vertex v |> G.label).token_sol))
        vars_vertex
    in
    sol
end
