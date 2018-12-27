open Kaputt.Abbreviations

type test = [`AvailableExpressions | `LiveVariables | `ReachingDefinitions
            | `VeryBusyExpressions | `SignAnalysis]

let test t =
  let f,p = match t with
      `AvailableExpressions -> "ae","available"
    | `LiveVariables        -> "lv","livevars"
    | `ReachingDefinitions  -> "rd","reaching"
    | `VeryBusyExpressions  -> "vb","vbusy"
    | `SignAnalysis         -> "signs","sign"
  in let tmp_result = Shell.temp_file f ".result" in
  let cmd = Shell.redirect_output
      (Shell.command ("../exe/caoMain.exe --" ^ p ^ " ../../../../tests/cao/" ^ f ^ ".cao"))
      tmp_result in
  let result = Shell.run cmd in
  if result <> 0 then begin
    print_endline ("failed to run test " ^ p);
    exit 2
  end else begin
    let diff = Shell.run (Shell.diff ~options:["-q"] ("../../../../tests/cao/" ^ f ^ ".result") tmp_result) in
    if diff <> 0 then begin
      print_endline ("result doesn't match " ^ p);
      exit 1
    end
  end

let () =
  test `AvailableExpressions;
  test `LiveVariables;
  test `ReachingDefinitions;
  test `VeryBusyExpressions;
  (*test `SignAnalysis; *)
  print_endline "all tests were successfull";
  exit 0
