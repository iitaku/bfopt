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
  let find_ljmp irs irp = 
    let rec find_ljmp_sub depth irs irp = 
      match (depth, irs.(irp)) with
        | (0, LJMP _) -> irp
        | (_, LJMP _) -> find_ljmp_sub (depth-1) irs (irp+1)
        | (_, RJMP _) -> find_ljmp_sub (depth+1) irs (irp+1)
        | (_, _)      -> find_ljmp_sub (depth) irs (irp+1) in
    find_ljmp_sub 0 irs irp in
  let find_rjmp irs irp = 
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
      | RJMP None -> (irs.(irp) <- RJMP (Some (find_ljmp irs (irp+1))); resolve irs (irp+1))
      | LJMP None -> (irs.(irp) <- LJMP (Some (find_rjmp irs (irp-1))); resolve irs (irp+1))
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

let hello = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."
let test = "[++++>++++>]"

(*****************
 ** Interpreter **
 *****************)

(** interpreter **)
let rec interp insts ip stack sp =
  let find_ljmp insts ip = 
    let rec find_ljmp_sub cnt insts ip = 
      (match cnt, insts.[ip] with
        | (0, '[') -> ip
        | (_, ']') -> find_ljmp_sub (cnt+1) insts (ip-1)
        | (_, '[') -> find_ljmp_sub (cnt-1) insts (ip-1)
        | (_,  _ ) -> find_ljmp_sub  cnt    insts (ip-1)) in
    find_ljmp_sub 0 insts ip in
  let rec find_rjmp insts ip =
    let rec find_rjmp_sub cnt insts ip = 
      (match cnt, insts.[ip] with
        | (0, ']') -> ip
        | (_, '[') -> find_rjmp_sub (cnt+1) insts (ip+1)
        | (_, ']') -> find_rjmp_sub (cnt-1) insts (ip+1)
        | (_,  _ ) -> find_rjmp_sub  cnt    insts (ip+1)) in
    find_rjmp_sub 0 insts ip in
  match insts.[ip] with
    | '>' -> interp insts (ip+1) stack (sp+1)
    | '<' -> interp insts (ip+1) stack (sp-1)
    | '+' -> let s = stack.(sp) + 1 in 
                (stack.(sp) <- s; interp insts (ip+1) stack sp)
    | '-' -> let s = stack.(sp) - 1 in 
                (stack.(sp) <- s; interp insts (ip+1) stack sp)
    | '.' -> let s = stack.(sp) in
                (print_char (Char.chr s); flush_all (); interp insts (ip+1) stack sp)
    | ',' -> let s = Char.code (input_char stdin) in 
                (stack.(sp) <- s; interp insts (ip+1) stack sp)
    | '[' -> (match stack.(sp) with
                | 0 -> interp insts ((find_rjmp insts (ip+1))+1) stack sp
                | _ -> interp insts (ip+1) stack sp)
    | ']' -> (match stack.(sp) with
                | 0 -> interp insts (ip+1) stack sp
                | _ -> interp insts ((find_ljmp insts (ip-1))+1) stack sp)
    |  _  -> interp insts (ip+1) stack sp

(** brainfuck **)
let brainfuck file_name = try interp (load file_name) 0 (Array.make 30000 0) 0 with Invalid_argument "index out of bounds" -> ()
    
(** main **)
let () = match Array.length Sys.argv with
           | 2 -> brainfuck (Sys.argv.(1))
           | _ -> invalid_arg "./bfopt [script.b]"
