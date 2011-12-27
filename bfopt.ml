type opcode = PINC | PDEC | VINC | VDEC | READ | WRITE | LJMP | RJMP

(** load file **)
let load file_name = 
    let rec load_sub fin = 
        let line = try [input_line fin] with End_of_file -> [] in
            match line with
                | [] -> ""
                | _  -> List.hd line ^ "\n" ^ load_sub fin
    in load_sub (open_in file_name)

let find_ljmp inst ip = 
    let rec find_ljmp_sub cnt inst ip = 
        (match cnt, String.get inst ip with
           | (0, '[') -> ip
           | (_, ']') -> find_ljmp_sub (cnt+1) inst (ip-1)
           | (_, '[') -> find_ljmp_sub (cnt-1) inst (ip-1)
           | (_,  _ ) -> find_ljmp_sub  cnt    inst (ip-1))
    in find_ljmp_sub 0 inst ip
        
let rec find_rjmp inst ip =
    let rec find_rjmp_sub cnt inst ip = 
        (match cnt, String.get inst ip with
           | (0, ']') -> ip
           | (_, '[') -> find_rjmp_sub (cnt+1) inst (ip+1)
           | (_, ']') -> find_rjmp_sub (cnt-1) inst (ip+1)
           | (_,  _ ) -> find_rjmp_sub  cnt    inst (ip+1))
    in find_rjmp_sub 0 inst ip

(** interpreter **)
let rec interp inst ip stack sp =
        match String.get inst ip with
          | '>' -> interp inst (ip+1) stack (sp+1)
          | '<' -> interp inst (ip+1) stack (sp-1)
          | '+' -> let s = (Array.get stack sp) + 1 in 
                      (Array.set stack sp s; interp inst (ip+1) stack sp)
          | '-' -> let s = (Array.get stack sp) - 1 in 
                      (Array.set stack sp s; interp inst (ip+1) stack sp)
          | '.' -> let s = (Array.get stack sp) in
                      (print_char (Char.chr s); flush_all (); interp inst (ip+1) stack sp)
          | ',' -> let s = Char.code (input_char stdin) in 
                      (Array.set stack sp s; interp inst (ip+1) stack sp)
          | '[' -> (match Array.get stack sp with
                      | 0 -> interp inst ((find_rjmp inst (ip+1))+1) stack sp
                      | _ -> interp inst (ip+1) stack sp)
          | ']' -> (match Array.get stack sp with
                      | 0 -> interp inst (ip+1) stack sp
                      | _ -> interp inst ((find_ljmp inst (ip-1))+1) stack sp)
          |  _  -> interp inst (ip+1) stack sp

(** brainfuck **)
let brainfuck file_name = try interp (load file_name) 0 (Array.make 30000 0) 0 with Invalid_argument "index out of bounds" -> ()
    
(** main **)
let main = 
    match Array.length Sys.argv with
        | 2 -> brainfuck (Array.get Sys.argv 1)
        | _ -> invalid_arg "./bfopt [script.b]";;

main;;
