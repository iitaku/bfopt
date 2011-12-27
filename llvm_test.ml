open Llvm

module E = Llvm_executionengine

let _ = E.initialize_native_target ()

let context = global_context ()

let module_ = create_module context "mymodule"

let builder = builder context

let i32_t = i32_type context

let double_type = function_type i32_t [| i32_t |]

let double = declare_function "double" double_type module_

let bb = append_block context "entry" double

let () = position_at_end bb builder

let param = match params double with
  | [| param |] -> param
  | _ -> assert false

let () = set_value_name "param" param

let doubled = build_mul param (const_int i32_t 2) "doubled" builder

let () = ignore (build_ret doubled builder)

let () = dump_value double

let () = Llvm_analysis.assert_valid_function double

let engine = E.ExecutionEngine.create module_

let res = E.ExecutionEngine.run_function double [| E.GenericValue.of_int i32_t 21|] engine

let res_int = E.GenericValue.as_int res

let () = Printf.eprintf "double(21)=%d\n" res_int

(** **)
