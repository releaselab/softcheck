open Batteries
open Cmdliner
open Tip

module Parser = Softcheck.Parsing_utils.Make(Ast)(Parser)(Lexer)

let show_cfg = Cfg.show % Cfg.generate_from_program

(* let show_icfg = Inter_cfg.show % Inter_cfg.generate_from_program *)

let ae p =
  let open Printf in
  print_endline "Available Expressions";
  let module AvailableExpressions = Available_expressions.Solve(struct let p = p end) in
  let open AvailableExpressions in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty p in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let rd p =
  let open Printf in
  print_endline "Reaching Definitions";
  let module ReachingDefinitions = Reaching_definitions.Solve(struct let p = p end) in
  let open ReachingDefinitions in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty p in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let vb p =
  let open Printf in
  print_endline "Very Busy Expressions";
  let module VeryBusyExpressions = Very_busy_expressions.Solve(struct let p = p end) in
  let open VeryBusyExpressions in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty p in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let lv p =
  let open Printf in
  print_endline "Live Variables";
  let module LiveVariables = Live_variables.Solve(struct let p = p end) in
  let open LiveVariables in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty p in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let sa p =
  let open Printf in
  print_endline "Sign";
  let module SignAnalysis = Sign.Solve(struct let p = p end) in
  let open SignAnalysis in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty p in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let ta p =
  let open Printf in
  print_endline "Taint Analysis";
  let module TaintAnalysis = Taint.Solve(struct let p = p end) in
  let open TaintAnalysis in
  let sinks = Set.fold (fun (l,b) acc -> match b with
      Tip.Flow.Soutput e -> (l,e) :: acc
    | _ -> acc) (Flow.blocks (List.hd p)) [] |> List.rev in
  let aux (l,e) =
    let result = snd (get_entry_result l) in
    let open Tip.Ast in
    let rec aux_rec = function
        Ecallf _
      | Ecallfptr _
      | Ecst _
      | Einput
      | Emalloc
      | Enull -> ()
      | Ebinop (_, e1, e2) -> aux_rec e1; aux_rec e2
      | Eunop (_, e) -> aux_rec e
      | Eident i -> match Map.find i result with
          Softcheck.Taint_lattice.Element true -> printf "Tainted information reaches sink in %d\n" l
        | Softcheck.Taint_lattice.Top -> printf "Tainted information may reach sink in %d\n" l
        | _ -> ()
    in aux_rec e in
  List.iter aux sinks

let run cfg _ livevars vbusy reaching available sign taint filename =
  let p = Parser.compile filename in
  if cfg then show_cfg p;
  (* if icfg then show_icfg p; *)
  if livevars then lv p;
  if vbusy then vb p;
  if reaching then rd p;
  if available then ae p;
  if sign then sa p;
  if taint then ta p

let command =
  let doc = "run monotone static analyses" in
  Term.(const run $
        Arg.(value & flag & info ["cfg"] ~doc:"construct the (intraprocedural) control-flow graph") $
        Arg.(value & flag & info ["icfg"] ~doc:"construct the interprocedural control-flow graph") $
        Arg.(value & flag & info ["livevars"] ~doc:"enable live variables analysis") $
        Arg.(value & flag & info ["vbusy"] ~doc:"enable very busy expressions analysis") $
        Arg.(value & flag & info ["reaching"] ~doc:"enable reaching definitions analysis") $
        Arg.(value & flag & info ["available"] ~doc:"enable available expressions analysis") $
        Arg.(value & flag & info ["sign"] ~doc:"enable sign analysis") $
        Arg.(value & flag & info ["taint"] ~doc:"enable taint analysis") $
        Arg.(required & pos ~rev:true 0 (some non_dir_file) None & info [] ~docv:"FILE")),
  Term.info "mframework" ~doc ~exits:Term.default_exits

let () = Term.(exit @@ eval command)
