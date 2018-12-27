open Batteries
open Utils
open Cmdliner

module Ast = Cao.Ast
module Flow = Cao.Flow
module Cfg = Cao.Cfg.Cfg
(*module InterCfg = Cao.InterCfg*)
module Parser = ParsingUtils.Make(Ast)(CaoParser)(CaoLexer)

let show_cfg = Cfg.show % Cfg.generate_from_program

(*let show_icfg = InterCfg.show % InterCfg.generate_from_program*)

let ae p =
  let open Printf in
  print_endline "Available Expressions";
  let module AvailableExpressions = AvailableExpressionsCao.Solve(struct let p = p end) in
  let open AvailableExpressions in
  let funcs = Ast.funcs_of_program p in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty funcs in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let lv p =
  let open Printf in
  print_endline "Live Variables";
  let module LiveVariables = LiveVariablesCao.Solve(struct let p = p end) in
  let open LiveVariables in
  let funcs = Ast.funcs_of_program p in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty funcs in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let rd p =
  let open Printf in
  print_endline "Reaching Definitions";
  let module ReachingDefinitions = ReachingDefinitionsCao.Solve(struct let p = p end) in
  let open ReachingDefinitions in
  let funcs = Ast.funcs_of_program p in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty funcs in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

let vb p =
  let open Printf in
  print_endline "Very Busy Expressions";
  let module VeryBusyExpressions = VeryBusyExpressionsCao.Solve(struct let p = p end) in
  let open VeryBusyExpressions in
  let funcs = Ast.funcs_of_program p in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty funcs in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels

(*let sa p =
  let open Printf in
  print_endline "Sign";
  let module SignAnalysis = SignAnalysisCao.Make(struct let p = p end) in
  let open SignAnalysis in
  let labels = List.fold_left (fun acc f -> Set.union (Flow.labels f) acc) Set.empty p in
  print_endline "Entry";
  Set.iter (fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string)) labels;
  print_endline "Exit";
  Set.iter (fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string)) labels*)

let run cfg (*icfg*) available livevars reaching vbusy (*sign*) filename =
  let p = Parser.compile filename in  
  if cfg then show_cfg p;
  (*if icfg then show_icfg p;*)
  if available then ae p;
  if livevars then lv p;
  if reaching then rd p;
  if vbusy then vb p
(*if sign then sa p*)

let command =
  let doc = "run monotone static analyses" in
  Term.(const run $
        Arg.(value & flag & info ["cfg"] ~doc:"construct the (intraprocedural) control-flow graph") $
        (*Arg.(value & flag & info ["icfg"] ~doc:"construct the interprocedural control-flow graph") $*)
        Arg.(value & flag & info ["available"] ~doc:"enable available expressions analysis") $
        Arg.(value & flag & info ["livevars"] ~doc:"enable live variables analysis") $
        Arg.(value & flag & info ["reaching"] ~doc:"enable reaching definitions analysis") $
        Arg.(value & flag & info ["vbusy"] ~doc:"enable very busy expressions analysis") $
        (*Arg.(value & flag & info ["sign"] ~doc:"enable sign analysis") $*)
        Arg.(required & pos ~rev:true 0 (some non_dir_file) None & info [] ~docv:"FILE")),
  Term.info "mframework" ~doc ~exits:Term.default_exits

let () = Term.(exit @@ eval command)
