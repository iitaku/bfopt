all: bfopt

bfopt: bfopt.cmx
	ocamlfind ocamlopt -package llvm.analysis,llvm.executionengine,llvm.scalar_opts,llvm.target -linkpkg -o $@ $<

llvm_test: llvm_test.cmx
	ocamlfind ocamlopt -package llvm.analysis,llvm.executionengine,llvm.scalar_opts,llvm.target -linkpkg -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -package llvm.analysis,llvm.executionengine,llvm.scalar_opts,llvm.target -linkpkg -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -package llvm.analysis,llvm.executionengine,llvm.scalar_opts,llvm.target -linkpkg -c $<

.PHONY: clean

clean:
	rm -f *.cm* *.o bfopt llvm_test 
