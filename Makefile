%.cmi: %.mli
	ocamlc $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

sieve.cma: sieve.cmo
	ocamlc -a $< -o $@

clean:
	rm -rf *.cmi *.cma *.cmo *.cmx *.cmxa *.o

.PHONY: clean
.PRECIOUS: %.cmi
