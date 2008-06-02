OCAMLBUILD:=ocamlbuild

byte: generate
	$(OCAMLBUILD) main.byte

test: unit

unit: generate camlp4/TestCaseCollector.cmo
	rm -f _build/src/asm.cm[oi]
	$(OCAMLBUILD) runner.byte --

integrate:
	sh example/test.sh example/*.scm

generate: src/match.ml src/opcode.ml

src/match.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -m < util/instruction.txt > $@

src/opcode.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -t < util/instruction.txt > $@

clean:
	ocamlbuild -clean
	rm -f  *~ */*~ *.abc *.cm[io] */*.cm[io] src/match.ml src/opcode.{ml,mli}

%.cmo:
	$(OCAMLBUILD) $@

%.byte:
	$(OCAMLBUILD) $@
