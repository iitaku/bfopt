open Llvm

module E = Llvm_executionengine

let _ = E.initialize_native_target ()

let context = global_context ()

let module_ = create_module context "mymodule"

let builder = builder context
