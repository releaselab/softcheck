open Batteries

type ast_stmt = Ast.stmt

and label = int

type program = Ast.program

type func = Ast.func

type ident = string

type expr = Ast.expr

type block =
  | VDecl of Ast.varDecl
  | CDecl of Ast.const_d
  | Assign of Ast.assign
  | Sample of Ast.lVal list
  | FCallS of Ast.fcalls
  | Ret of Ast.expr
  | Ite of Ast.expr
  | Seq of Ast.seqheader
  | While of Ast.expr

let rev_pair x y = (y, x)

let to_string = let open Printer in
  let seq_to_string s =
    let open Ast in
    let open Printer in
    Printf.sprintf "seq %s := %s to %s%s" s.seqheader_var
      (exp_to_string s.seqheader_start_val)
      (exp_to_string s.seqheader_end_val)
      (match s.seqheader_increase with
         None -> ""
       | Some e -> " by " ^ (exp_to_string e)) in
  function
    VDecl varDecl -> vardecl_to_string varDecl
  | CDecl const_d -> const_d_to_string const_d
  | Assign assign -> assign_to_string assign
  | Sample lVals -> sample_to_string lVals
  | FCallS fcalls -> fcalls_to_string fcalls
  | Ret expr -> "return " ^ exp_to_string expr
  | Ite expr -> Printf.sprintf "if (%s)" (exp_to_string expr)
  | Seq seq -> seq_to_string seq
  | While expr -> Printf.sprintf "while (%s)" (exp_to_string expr)

let rec init = let open Ast in function
    Stmt (l, _) -> l
  | Block b -> (init % List.first) b

let final = let open Ast in let open Set.Infix in
  let rec final_rec acc = function
      Stmt (l,s) -> (match s with
        VDecl _
      | CDecl _
      | Assign _
      | Sample _
      | FCallS _
      | Ret _
      | Seq _ -> acc <-- l
      | Ite ite -> final_rec (match ite.else_branch with
          None -> acc
        | Some e -> final_rec acc e) ite.if_branch
      | While loop -> final_rec acc loop.loop_body)
    | Block b -> final_rec acc (List.last b)
  in final_rec Set.empty

let blocks f = let open Set.Infix in
  let rec blocks_rec acc =
    function
      Ast.Stmt (l, s) ->
        (match s with
           Ast.VDecl varDecl -> acc <-- (l, VDecl varDecl)
         | Ast.CDecl const_d -> acc <-- (l, CDecl const_d)
         | Ast.Assign assign -> acc <-- (l, Assign assign)
         | Ast.Sample lVals -> acc <-- (l, Sample lVals)
         | Ast.FCallS fcalls -> acc <-- (l, FCallS fcalls)
         | Ast.Ret expr -> acc <-- (l, Ret expr)
         | Ast.Ite ite -> let acc2 = acc <-- (l, Ite ite.Ast.ite_condition) in
             let acc3 = match ite.Ast.else_branch with
                 None -> acc2
               | Some e -> blocks_rec acc2 e in
             blocks_rec acc3 ite.Ast.if_branch
         | Ast.Seq seq ->
             blocks_rec (acc <-- (l, Seq seq.Ast.seq_header)) seq.Ast.seq_body
         | Ast.While loop ->
             blocks_rec (acc <-- (l, While loop.Ast.loop_condition)) loop.Ast.loop_body)
    | Ast.Block b -> List.fold_left blocks_rec acc b
  in blocks_rec Set.empty f.Ast.func_body

let labels = Set.map fst % blocks

let flow f = let open Set.Infix in let open Ast in
  let rec flow_rec acc = function
      Stmt (l, s) -> (match s with
        VDecl _
      | CDecl _
      | Assign _
      | Sample _
      | Ret _
      | FCallS _ -> acc
      | Seq seq ->
          let s = Set.map (rev_pair l) (final seq.seq_body) <-- (l,init seq.seq_body) in
          flow_rec (Set.union s acc) seq.seq_body
      | Ite ite -> let acc2 = acc <-- (l, init ite.if_branch) in
          let acc3 = match ite.else_branch with
              None -> acc2
            | Some e -> flow_rec (acc2 <-- (l, init e)) e in
          flow_rec acc3 ite.if_branch
      | While loop ->
          let s = Set.map (rev_pair l) (final loop.loop_body) <-- (l,init loop.loop_body) in
          flow_rec (Set.union s acc) loop.loop_body)
    | Block b -> flow_block acc b
  and flow_block acc = function
      []            -> assert false
    | [s]           -> flow_rec acc s
    | s1 :: s2 :: b -> let init_s2 = init s2 in
        let final_s1 = final s1 in
        flow_block (flow_rec (flow_rec (Set.union acc (Set.map (rev_pair init_s2) final_s1)) s1) s2) (s2 :: b)
  in flow_rec Set.empty f.func_body

let flowR = Set.map (fun (a,b) -> (b,a)) % flow
