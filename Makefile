%.cmi: %.mli
	ocamlc $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<

%: %.ml
	ocaml $<

prob7: sieve.cmo
prob10: sieve.cmo
prob27: sieve.cmo

clean:
	rm -rf *.cmi *.cma *.cmo *.cmx *.cmxa *.o

.PHONY: clean
.PRECIOUS: %.cmi
