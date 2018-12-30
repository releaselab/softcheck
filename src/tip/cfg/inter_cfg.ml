open Batteries
open Softcheck

include Flow_graph.MakeInterCfg(Inter_flow)

let generate_from_program (p : Ast.program) =
  let open Ast in
  let open Inter_flow in
  let graph = create() in (* graph -> Make2.t *)
  let pBlocks = Hashtbl.create 10 in (* pBlocks -> Hashtbl.t *)
  let s_p = convert_to_simple_form p in
  s_p |> List.map (fun f -> init_f f) |> List.iter (extremal graph);
  (* Define inter-flow *)
  inter_flow_p s_p |> Set.iter (interflow graph);
  (* Add the entry label of each function to the extremals of graph1 *)
  List.iter (fun f -> Hashtbl.replace pBlocks f.s_func_id (blocks_f f)) s_p;
  (* Add the blocks of each function to pBlocks (id -> blocks) *)
  Hashtbl.iter (fun func_id blocks -> Set.iter (fun (l,b) -> add graph func_id (l,b)) blocks) pBlocks;
  let add_edge (i, j) = connect graph i j in (* Receives two labels to connect *)
  flow_p s_p |> Set.iter add_edge;
  graph

let callees g l =
  outflow g l |>
  List.filter (fun x -> let open Inter_flow in match (get g x) with
    Sbegin _        -> true
  | Sassign _ | Scallassign _ | Saftercallassign _ | Sif _ | Soutput _
  | Swhile _ | Send | Sreturn _ -> false)

