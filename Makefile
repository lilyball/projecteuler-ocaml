%.cmi: %.mli
	ocamlc $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<

%: %.ml
	ocaml $<

clean:
	rm -rf *.cmi *.cma *.cmo *.cmx *.cmxa *.o

.PHONY: clean
.PRECIOUS: %.cmi
