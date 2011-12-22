type opcode = PINC | PDEC | VINC | VDEC | READ | WRITE | LJMP | RJMP

(** load file **)
let load file_name = 
    let rec load_sub fin = 
        let line = try [input_line fin] with End_of_file -> [] in
            match line with
                | [] -> ""
                | _  -> List.hd line ^ "\n" ^ load_sub fin
    in load_sub (open_in file_name);;

(** interpreter **)
let interp instruction ip stack sp = eval 

(** brainfuck **)
let brainfuck file_name = interp (load file_name) 0 (Array.make 30000 0) 0;;
    
(** main **)
let main = 
    match Array.length Sys.argv with
        | 2 -> brainfuck (Array.get Sys.argv 1)
        | _ -> invalid_arg "./bfopt [script.b]";;

main;;
