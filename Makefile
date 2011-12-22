bfopt: bfopt.cmx
	ocamlfind ocamlopt -package llvm.executionengine -linkpkg -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -package llvm.executionengine -linkpkg -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -package llvm.executionengine -linkpkg -c $<

.PHONY: clean

clean:
	rm -f *.cm* *.o bfopt
