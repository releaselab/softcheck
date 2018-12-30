open Batteries
open Kaputt
open Softcheck
open Utils

module CSolver = Cubic.Make
    (struct type t = string let compare = compare let to_string s = s end)
    (struct type t = string let n = 15 let to_string s = s end)

let () =
  let open CSolver in
  let () = add_constraint (Constant ("inc", "inc")) in
  let () = add_constraint (Constant ("dec", "dec")) in
  let () = add_constraint (Constant ("ide", "ide")) in
  let () = add_constraint (Subset ("ide", "f")) in
  let () = add_constraint (Subset ("f(n)", "r")) in
  let () = add_constraint (Conditional (("inc", "f"),("n", "i"))) in
  let () = add_constraint (Conditional (("inc", "f"),("i+1", "f(n)"))) in
  let () = add_constraint (Conditional (("dec", "f"),("n", "j"))) in
  let () = add_constraint (Conditional (("dec", "f"),("j-1", "f(n)"))) in
  let () = add_constraint (Conditional (("ide", "f"),("n", "k"))) in
  let () = add_constraint (Conditional (("ide", "f"),("k","f(n)"))) in
  let () = add_constraint (Subset ("input", "x")) in
  let () = add_constraint (Subset ("foo(x,inc)", "y")) in
  let () = add_constraint (Subset ("foo(x,dec)", "y")) in
  let () = add_constraint (Constant ("foo", "foo")) in
  let () = add_constraint (Conditional (("foo", "foo"),("x", "n"))) in
  let () = add_constraint (Conditional (("foo", "foo"),("inc", "f"))) in
  let () = add_constraint (Conditional (("foo", "foo"),("f(n)","foo(x,inc)"))) in
  let () = add_constraint (Conditional (("foo", "foo"),("x", "n"))) in
  let () = add_constraint (Conditional (("foo", "foo"),("dec", "f"))) in
  let () = add_constraint (Conditional (("foo", "foo"),("f(n)", "foo(x,dec)"))) in
  let () = add_constraint (Constant ("main", "main")) in
  let solution = get_solution () in
  let result_string = Hashtbl.fold (fun k v acc ->
    (Printf.sprintf "%s: %s" (CSolver.var_to_string k) (sprint_list ~first:"{" ~last:"}" ~sep:", " CSolver.token_to_string v) :: acc)) solution [] in
  let solution_string = [
    "i: {}";
    "ide: {ide}";
    "r: {}";
    "x: {}";
    "input: {}";
    "y: {}";
    "f: {ide, dec, inc}";
    "foo(x,dec): {}";
    "j: {}";
    "inc: {inc}";
    "n: {}";
    "foo: {foo}";
    "i+1: {}";
    "j-1: {}";
    "main: {main}";
    "f(n): {}";
    "dec: {dec}";
    "k: {}";
    "foo(x,inc): {}"
  ] in
  let tests = List.fold_left2 (fun acc e1 e2 ->
    Test.make_simple_test ~title:"cubic" (fun () ->Assertion.equal e1 e2) :: acc) [] result_string solution_string in
  let tests_result = Test.exec_tests tests in
  if List.for_all (function Test.Passed -> true | _ -> false) tests_result then
    exit 0
  else
    exit 1
