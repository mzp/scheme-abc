OCAMLBUILD:=ocamlbuild

byte: generate
	$(OCAMLBUILD) main.byte

test: generate camlp4/TestCaseCollector.cmo
	$(OCAMLBUILD) runner.byte --

generate: src/match_core.ml src/opcode.ml src/opcode.mli

src/match_core.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -m < util/instruction.txt > $@

src/opcode.mli : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -i < util/instruction.txt > $@

src/opcode.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -t < util/instruction.txt > $@

clean:
	ocamlbuild -clean
	rm -f  *~ */*~ *.abc *.cm[io] */*.cm[io] src/match_core.ml src/opcode.{ml,mli}

%.cmo:
	$(OCAMLBUILD) $@

%.byte:
	$(OCAMLBUILD) $@
