all: bfopt llvm_test

bfopt: bfopt.cmx
	ocamlfind ocamlopt -package llvm.analysis,llvm.executionengine -linkpkg -o $@ $<

llvm_test: llvm_test.cmx
	ocamlfind ocamlopt -package llvm.analysis,llvm.executionengine -linkpkg -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -package llvm.analysis,llvm.executionengine -linkpkg -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -package llvm.analysis,llvm.executionengine -linkpkg -c $<

.PHONY: clean

clean:
	rm -f *.cm* *.o bfopt
