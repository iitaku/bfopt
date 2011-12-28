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

(** find jmp address **)
let find_jmpl inst ip = 
  let rec find_jmpl_sub cnt inst ip = 
    (match cnt, inst.[ip] with
      | (0, '[') -> ip
      | (_, ']') -> find_jmpl_sub (cnt+1) inst (ip-1)
      | (_, '[') -> find_jmpl_sub (cnt-1) inst (ip-1)
      | (_,  _ ) -> find_jmpl_sub  cnt    inst (ip-1)) in
  find_jmpl_sub 0 inst ip
        
let rec find_jmpr inst ip =
  let rec find_jmpr_sub cnt inst ip = 
    (match cnt, inst.[ip] with
      | (0, ']') -> ip
      | (_, '[') -> find_jmpr_sub (cnt+1) inst (ip+1)
      | (_, ']') -> find_jmpr_sub (cnt-1) inst (ip+1)
      | (_,  _ ) -> find_jmpr_sub  cnt    inst (ip+1)) in
  find_jmpr_sub 0 inst ip

(***********************
 ** IR VirtualMachine **
 ***********************)
type ir_t = SHFT of int | VARY of int | WRTE | READ | RJMP of int option | LJMP of int option | NOP

let i2ir i = function
  | '>' -> SHFT  1
  | '<' -> SHFT -1
  | '+' -> VARY  1
  | '-' -> VARY -1
  | '.' -> WRTE
  | ',' -> READ
  | '[' -> RJMP None
  | ']' -> LJMP None

let fuse (lhs, rhs) = function
  | ((SHFT l), (SHFT r)) -> Some (SHFT (l+r))
  | ((VARY l), (VARY r)) -> Some (VARY (l+r))
  | _ -> None

let inst2ir inst = 
  let rec inst2ir_sub inst ip ir = 
    let i = try inst.[ip] with Invalid_argument -> ir in
    
    match fuse (List.hd ir) r with
      | ('>', (SHFT n)::rest_ir) -> inst2ir_sub inst (ip+1) (SHFT (n+1))
      | ('>', )
      | ('<', (SHFT n)::rest_ir) -> inst2ir_sub inst (ip+1) (SHFT (n-1))
      | ('+', (VARY n)::rest_ir) -> inst2ir_sub inst (ip+1) (VARY (n+1))
      | ('-', (VARY n)::rest_ir) -> inst2ir_sub inst (ip+1) (VARY (n-1))
      | ('.', ir    ) -> inst2ir_sub inst (ip+1) (
                  (print_char (Char.chr s); flush_all (); interp inst (ip+1) stack sp)
      | (',' -> let s = Char.code (input_char stdin) in 
                  (stack.(sp) <- s; interp inst (ip+1) stack sp)
      | ('[' -> (match stack.(sp) with
                  | 0 -> interp inst ((find_jmpr inst (ip+1))+1) stack sp
                  | _ -> interp inst (ip+1) stack sp)
      | (']' -> (match stack.(sp) with
                  | 0 -> interp inst (ip+1) stack sp
                  | _ -> interp inst ((find_jmpl inst (ip-1))+1) stack sp)
      |  _  -> interp inst (ip+1) stack sp in
  let rec reconstruct_ir ir = function
      | []               -> []
      |  NOP    ::rest_ir -> reconstruct_ir rest_ir
      | (SHFT 0)::rest_ir -> reconstruct_ir rest_ir
      | (VARY 0)::rest_ir -> reconstruct_ir rest_ir
      |  ir     ::rest_ir -> ir :: reconstruct_ir rest_ir in
  let rev_ir = inst2ir_sub inst 0 [] in
  List.rev (reconstruct_ir rev_ir)

(*****************
 ** Interpreter **
 *****************)

(** interpreter **)
let rec interp inst ip stack sp =
  match inst.[ip] with
    | '>' -> interp inst (ip+1) stack (sp+1)
    | '<' -> interp inst (ip+1) stack (sp-1)
    | '+' -> let s = stack.(sp) + 1 in 
                (stack.(sp) <- s; interp inst (ip+1) stack sp)
    | '-' -> let s = stack.(sp) - 1 in 
                (stack.(sp) <- s; interp inst (ip+1) stack sp)
    | '.' -> let s = stack.(sp) in
                (print_char (Char.chr s); flush_all (); interp inst (ip+1) stack sp)
    | ',' -> let s = Char.code (input_char stdin) in 
                (stack.(sp) <- s; interp inst (ip+1) stack sp)
    | '[' -> (match stack.(sp) with
                | 0 -> interp inst ((find_jmpr inst (ip+1))+1) stack sp
                | _ -> interp inst (ip+1) stack sp)
    | ']' -> (match stack.(sp) with
                | 0 -> interp inst (ip+1) stack sp
                | _ -> interp inst ((find_jmpl inst (ip-1))+1) stack sp)
    |  _  -> interp inst (ip+1) stack sp

(** brainfuck **)
let brainfuck file_name = try interp (load file_name) 0 (Array.make 30000 0) 0 with Invalid_argument "index out of bounds" -> ()
    
(** main **)
let () = match Array.length Sys.argv with
           | 2 -> brainfuck (Sys.argv.(1))
           | _ -> invalid_arg "./bfopt [script.b]"
