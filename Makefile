%.cmi: %.mli
	ocamlc $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<

%: %.ml
	ocamlopt -o a.out $(wordlist 2, $(words $^), $^) $<
	@echo ./a.out
	@trap 'rm a.out' EXIT && ./a.out

nums.cmxa:
str.cmxa:

prob1: nums.cmxa misc.cmx
prob3: nums.cmxa misc.cmx
prob5: nums.cmxa misc.cmx
prob6: nums.cmxa misc.cmx
prob7: sieve.cmx
prob8: str.cmxa
prob10: nums.cmxa sieve.cmx
prob12: nums.cmxa misc.cmx
prob13: nums.cmxa misc.cmx
prob14: nums.cmxa
prob15: nums.cmxa
prob16: nums.cmxa
prob20: nums.cmxa
prob21: nums.cmxa misc.cmx
prob22: nums.cmxa str.cmxa
prob23: nums.cmxa misc.cmx
prob24: nums.cmxa misc.cmx
prob25: nums.cmxa
prob26: nums.cmxa
prob27: sieve.cmx nums.cmxa misc.cmx
prob29: nums.cmxa misc.cmx

clean:
	rm -rf *.cmi *.cma *.cmo *.cmx *.cmxa *.o

.PHONY: clean
.PRECIOUS: %.cmi
