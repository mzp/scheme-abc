LIB:=extlib,ounit
OCAMLC:=ocamlfind ocamlc -linkpkg -package $(LIB)
OCAMLBUILD:=ocamlbuild -ocamlc '$(OCAMLC)'

byte: generate
	$(OCAMLBUILD) main.byte

test: camlp4/TestCaseCollector.cmo test/runner.byte generate
	./_build/test/runner.byte

generate: src/match.ml src/types.ml src/types.mli

src/match.ml : util/instruction.byte util/instruction.txt
	./instruction.byte -m < util/instruction.txt > $@

src/types.mli : util/instruction.byte util/instruction.txt
	./instruction.byte -i < util/instruction.txt > $@

src/types.ml : util/instruction.byte util/instruction.txt
	./instruction.byte -t < util/instruction.txt > $@

clean:
	ocamlbuild -clean
	rm -f  *~ */*~ *.abc *.cm[io] */*.cm[io]

%.cmo:
	$(OCAMLBUILD) $@

%.byte:
	$(OCAMLBUILD) $@
