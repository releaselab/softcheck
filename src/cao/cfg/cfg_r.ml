open Batteries
open Softcheck

include Flow_graph.Make_cfg(Flow)

type program = Flow.program

let generate_from_program (p : Ast.program) =
  let open Ast in
  let graph = create() in (* graph -> Make2.t *)
  let pBlocks = Hashtbl.create 10 in (* pBlocks -> Hashtbl.t *)
  let funcs = Ast.funcs_of_program p in
  funcs |> List.map (fun f -> Flow.final f.func_body) |> List.iter (Set.iter (extremal graph));
  (* Add the entry label of each function to the extremals of graph1 *)
  List.iter (fun f -> Hashtbl.replace pBlocks f.func_id (Flow.blocks f)) funcs;
  (* Add the blocks of each function to pBlocks (id -> blocks) *)
  Hashtbl.iter (fun id blocks -> Set.iter (add graph id) blocks) pBlocks;
  let add_edge (i, j) = connect graph i j in (* Receives two labels to connect *)
  List.iter (fun f -> Flow.flowR f |> Set.iter add_edge) funcs;
  graph
