open Batteries
open Softcheck

include Flow_graph.Make_cfg(Flow)

type program = Flow.program

let generate_from_program (p : Ast.program) =
  let open Ast in
  let graph = create() in (* graph -> Make2.t *)
  let pBlocks = Hashtbl.create 10 in (* pBlocks -> Hashtbl.t *)
  let funcs = match p.main with
      None -> List.filter_map (function FunDef f -> Some f | _ -> None) p.defs
    | Some m -> m :: List.filter_map (function FunDef f -> Some f | _ -> None) p.defs
  in
  funcs |> List.map (fun f -> Flow.init f.func_body) |> List.iter (extremal graph);
  funcs |> List.map (fun f -> Flow.final f.func_body) |> List.iter (Set.iter (extremalR graph));
  (* Add the entry and final labels of each function to the respective extremals of the graph *)
  List.iter (fun f -> Hashtbl.replace pBlocks f.func_id (Flow.blocks f)) funcs;
  (* Add the blocks of each function to pBlocks (id -> blocks) *)
  Hashtbl.iter (fun id blocks -> Set.iter (add graph id) blocks) pBlocks;
  let add_edge (i, j) = connect graph i j in (* Receives two labels to connect *)
  List.iter (fun f -> Flow.flow f |> Set.iter add_edge) funcs;
  graph
