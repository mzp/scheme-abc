OCAMLBUILD:=ocamlbuild

byte: generate
	$(OCAMLBUILD) main.byte

test: generate camlp4/TestCaseCollector.cmo
	$(OCAMLBUILD) runner.byte --

generate: src/match.ml src/types.ml src/types.mli

src/match.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -m < util/instruction.txt > $@

src/types.mli : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -i < util/instruction.txt > $@

src/types.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -t < util/instruction.txt > $@

clean:
	ocamlbuild -clean
	rm -f  *~ */*~ *.abc *.cm[io] */*.cm[io] src/match.ml src/type.{ml,mli}

%.cmo:
	$(OCAMLBUILD) $@

%.byte:
	$(OCAMLBUILD) $@
