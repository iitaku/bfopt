(************
 ** Common **
 ************)

(** load file **)
let load file_name = 
  let rec load_sub fin = 
    let line = try [input_line fin] with End_of_file -> [] in
      match line with
        | [] -> ""
        | _  -> List.hd line ^ "\n" ^ load_sub fin in
    load_sub (open_in file_name)

let find_ljmp insts ip = 
  let rec find_ljmp_sub depth insts ip = 
    (match depth, insts.[ip] with
      | (0, '[') -> ip
      | (_, ']') -> find_ljmp_sub (depth+1) insts (ip-1)
      | (_, '[') -> find_ljmp_sub (depth-1) insts (ip-1)
      | (_,  _ ) -> find_ljmp_sub  depth    insts (ip-1)) in
  find_ljmp_sub 0 insts ip
let rec find_rjmp insts ip =
  let rec find_rjmp_sub depth insts ip = 
    (match depth, insts.[ip] with
      | (0, ']') -> ip
      | (_, '[') -> find_rjmp_sub (depth+1) insts (ip+1)
      | (_, ']') -> find_rjmp_sub (depth-1) insts (ip+1)
      | (_,  _ ) -> find_rjmp_sub  depth    insts (ip+1)) in
  find_rjmp_sub 0 insts ip

(*****************
 ** Interpreter **
 *****************)
(** interpreter **)
let rec interp_script insts ip stack sp =
 match insts.[ip] with
    | '>' -> interp_script insts (ip+1) stack (sp+1)
    | '<' -> interp_script insts (ip+1) stack (sp-1)
    | '+' -> (stack.(sp) <- stack.(sp) + 1; interp_script insts (ip+1) stack sp)
    | '-' -> (stack.(sp) <- stack.(sp) - 1; interp_script insts (ip+1) stack sp)
    | '.' -> (print_char (Char.chr stack.(sp)); flush_all (); interp_script insts (ip+1) stack sp)
    | ',' -> (stack.(sp) <- (Char.code (input_char stdin)); interp_script insts (ip+1) stack sp)
    | '[' -> (match stack.(sp) with
               | 0 -> interp_script insts ((find_rjmp insts (ip+1))+1) stack sp
               | _ -> interp_script insts (ip+1) stack sp)
    | ']' -> (match stack.(sp) with
               | 0 -> interp_script insts (ip+1) stack sp
               | _ -> interp_script insts ((find_ljmp insts (ip-1))+1) stack sp)
    |  _  -> interp_script insts (ip+1) stack sp

(***********************
 ** IR VirstualMachine **
 ***********************)
type ir_t = SHFT of int | VARY of int | WRTE | READ | RJMP of int option | LJMP of int option | NOP

let insts2irs insts = 
  let i2ir = function
    | '>' -> SHFT   1
    | '<' -> SHFT (-1)
    | '+' -> VARY   1
    | '-' -> VARY (-1)
    | '.' -> WRTE
    | ',' -> READ
    | '[' -> RJMP None
    | ']' -> LJMP None
    |  _  -> NOP in
  let fuse lhs rhs = match (lhs, rhs) with
    | ((SHFT l), (SHFT r)) -> if 0 = (l+r) then Some NOP else Some (SHFT (l+r))
    | ((VARY l), (VARY r)) -> if 0 = (l+r) then Some NOP else Some (VARY (l+r))
    | (NOP, rhs) -> Some rhs
    | (lhs, NOP) -> Some lhs
    | _ -> None in
  let find_ljmp_ir irs irp = 
    let rec find_ljmp_sub depth irs irp = 
      match (depth, irs.(irp)) with
        | (0, LJMP _) -> irp
        | (_, LJMP _) -> find_ljmp_sub (depth-1) irs (irp+1)
        | (_, RJMP _) -> find_ljmp_sub (depth+1) irs (irp+1)
        | (_, _)      -> find_ljmp_sub (depth) irs (irp+1) in
    find_ljmp_sub 0 irs irp in
  let find_rjmp_ir irs irp = 
    let rec find_rjmp_sub depth irs irp = 
      match (depth, irs.(irp)) with
        | (0, RJMP _) -> irp
        | (_, RJMP _) -> find_rjmp_sub (depth-1) irs (irp-1)
        | (_, LJMP _) -> find_rjmp_sub (depth+1) irs (irp-1)
        | (_, _)      -> find_rjmp_sub (depth) irs (irp-1) in
    find_rjmp_sub 0 irs irp in
  let rec resolve irs irp = 
    if irp = (Array.length irs) then irs else
    match irs.(irp) with
      | RJMP None -> (irs.(irp) <- RJMP (Some (find_ljmp_ir irs (irp+1))); resolve irs (irp+1))
      | LJMP None -> (irs.(irp) <- LJMP (Some (find_rjmp_ir irs (irp-1))); resolve irs (irp+1))
      | _         -> resolve irs (irp+1) in
  let rec convert insts ip irs = 
    if ip = (String.length insts) then irs else
    let ir = i2ir insts.[ip] in
    match fuse (List.hd irs) ir with
      | None     -> convert insts (ip+1) (ir::irs)
      | Some fir -> convert insts (ip+1) (fir::(List.tl irs)) in
  let reverse_irs = convert insts 0 [NOP] in
  let obverse_irs = List.rev reverse_irs in 
  resolve (Array.of_list obverse_irs) 0

let rec interp_ir irs irp stack sp = 
  match irs.(irp) with
    | SHFT n -> interp_ir irs (irp+1) stack (sp+n)
    | VARY n -> (stack.(sp) <- stack.(sp) + n; interp_ir irs (irp+1) stack sp)
    | WRTE   -> (print_char (Char.chr stack.(sp)); flush_all (); interp_ir irs (irp+1) stack sp)
    | READ   -> (stack.(sp) <- (Char.code (input_char stdin)); interp_ir irs (irp+1) stack sp)
    | RJMP Some n -> (match stack.(sp) with
                  | 0 -> interp_ir irs (n+1) stack sp
                  | _ -> interp_ir irs (irp+1) stack sp)
    | LJMP Some n -> interp_ir irs n stack sp
    | NOP    -> interp_ir irs (irp+1) stack sp
    | _      -> invalid_arg "invalid IR"

(******************************
 ** LLVM JIT Compile and Run **
 ******************************)
type ast_t = Node of (ast_t * ast_t * ast_t) | Leaf of ir_t array

open Llvm

module E = Llvm_executionengine
module T = Llvm_target
module S = Llvm_scalar_opts

let compile_and_run script = 
  
  (* init *)
  ignore (E.initialize_native_target ());
  let c = global_context () in
  let m = create_module c "bf_module" in
  let b = builder c in
  let e = E.ExecutionEngine.create m in
  
  (* bind type *)
  let i1_t = i1_type c in
  let i8_t = i8_type c in
  let i32_t = i32_type c in
  let void_t = void_type c in
  
  (* external func *)
  let getchar = declare_function "getchar" (function_type i32_t [||]) m in
  let putchar = declare_function "putchar" (function_type i32_t [| i8_t |]) m in
  let memset = declare_function "llvm.memset.p0i8.i32" (function_type void_t [| pointer_type i8_t; i8_t; i32_t; i32_t; i1_t |]) m in
  
  (* internal func *)
  let bfengine = declare_function "bfengine" (function_type void_t [| i32_t |]) m in
  
  (* entry point *)
  let bb = append_block c "" bfengine in
  position_at_end bb b;

  (* initialize stack *)
  let param = match params bfengine with
    | [| p |] -> p
    | _ -> assert false in
  let llstack = build_array_alloca i8_t param "stack" b in
  ignore (build_call memset [|llstack; const_int i8_t 0; param; const_int i32_t 16; const_int i1_t 0|] "" b);

  (* initialize stack pointer *)
  let llsp = build_alloca i32_t "sp" b in
  ignore (build_store (const_int i32_t 0) llsp b);
  
  (* parser *)
  let parse script = 
    let rec construct script snipped pos len1 =
      if len1 = pos then Leaf (insts2irs snipped) else
      match script.[pos] with
        | '[' -> 
          let mark = (find_rjmp script (pos+1)) in
          let len2 = mark - (pos+1) in
          let len3 = len1 - (mark+1) in
          let ast1 = Leaf (insts2irs snipped) in
          let ast2 = construct (String.sub script (pos+1) len2) "" 0 len2 in
          let ast3 = construct (String.sub script (mark+1) len3) "" 0 len3 in
          Node (ast1, ast2, ast3)
        | ']' -> assert false
        |  i  -> construct script (snipped ^ (Char.escaped i)) (pos+1) len1 in
    construct script "" 0 (String.length script) in
  
  (* code generator *)
  let rec codegen irs irp len = 
    if irp = len then () else
    match irs.(irp) with
      | SHFT n -> 
        let v1 = build_load llsp "" b in
        let v2 = build_add v1 (const_int i32_t n) "" b in
        let _ = build_store v2 llsp b in
        codegen irs (irp+1) len
      | VARY n -> 
        let p = build_gep llstack [| build_load llsp "" b |] "" b in
        let v1 = build_load p "" b in
        let v2 = build_add v1 (const_int i8_t n) "" b in
        let _ = build_store v2 p b in
        codegen irs (irp+1) len
      | WRTE   -> 
        let p = build_gep llstack [| build_load llsp "" b |] "" b in
        let v = build_load p "" b in
        let _ = build_call putchar [| v |] "" b in
        codegen irs (irp+1) len
      | READ   -> 
        let v1 = build_call getchar [||] "" b in
        let v2 = build_trunc_or_bitcast v1 i8_t "" b in
        let p = build_gep llstack [| build_load llsp "" b |] "" b in
        let _ = build_store v2 p b in
        codegen irs (irp+1) len
      | NOP    -> codegen irs (irp+1) len
      | _      -> invalid_arg "invalid IR" in
  
  (* traverse AST *)
  let rec traverse = function
      | Node (node1, node2, node3) ->
        (* blocks *)
        let bb1 = append_block c "" bfengine in
        let bb2 = append_block c "" bfengine in
        let bb3 = append_block c "" bfengine in
        
        (* before loop  *)
        let _ = traverse node1 in
        let _ = build_br bb1 b in
               
        (* loop header *)
        let () = position_at_end bb1 b in
        let v1 = const_int i8_t 0 in
        let p = build_gep llstack [| build_load llsp "" b |] "" b in
        let v2 = build_load p "" b in
        let cond = build_icmp Icmp.Ne v1 v2 "" b in
        let _ = build_cond_br cond bb2 bb3 b in
     
        (* loop body *)
        let () = position_at_end bb2 b in
        let _ = traverse node2 in
        let _ = build_br bb1 b in
        
        (* after loop *)
        let () = position_at_end bb3 b in
        let _ = traverse node3 in
        ()
      | Leaf irs -> codegen irs 0 (Array.length irs) in
  
  (* JIT compile *)
  traverse (parse script);
  ignore (build_ret_void b);
  (*dump_value bfengine;*)
  
  (* optimize *)
  let fpm = PassManager.create_function m in
  T.TargetData.add (E.ExecutionEngine.target_data e) fpm;
  S.add_scalar_repl_aggregation_ssa fpm; (*30*)
  ignore (PassManager.initialize fpm);
  ignore (PassManager.run_function bfengine fpm);
  
  (* dump and test *)
  (*dump_value bfengine;*)
  Llvm_analysis.assert_valid_function bfengine;
    
  (* execution *)
  ignore (E.ExecutionEngine.run_function bfengine [| E.GenericValue.of_int i32_t 30000 |] e);
  
  (* finalize *)
  E.ExecutionEngine.dispose e
  ;;

(** brainfuck **)
(*
let brainfuck file_name = try interp_script (load file_name) 0 (Array.make 30000 0) 0 with Invalid_argument "index out of bounds" -> ()
let brainfuck file_name = try interp_ir (insts2irs (load file_name)) 0 (Array.make 30000 0) 0 with Invalid_argument "index out of bounds" -> ()
*)
let brainfuck file_name = compile_and_run (load file_name)
    
(** main **)
let () = match Array.length Sys.argv with
           | 2 -> brainfuck (Sys.argv.(1))
           | _ -> invalid_arg "./bfopt [script.b]"
