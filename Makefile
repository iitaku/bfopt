test: test.cmx
	ocamlfind ocamlopt -package llvm -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -package llvm -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -package llvm -c $<

.PHONY: clean

clean:
	rm -f *.cm* *.o test
