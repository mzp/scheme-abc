LIB:=extlib,ounit
OCAMLC:=ocamlfind ocamlc -linkpkg -package $(LIB)
OCAMLBUILD:=ocamlbuild -ocamlc '$(OCAMLC)'

byte:
	$(OCAMLBUILD) main.byte

test: camlp4/TestCaseCollector.cmo test/runner.byte
	./_build/test/runner.byte

clean:
	ocamlbuild -clean
	rm -f  *~ */*~ *.abc *.cm[io] */*.cm[io]

%.cmo:
	$(OCAMLBUILD) $@

%.byte:
	$(OCAMLBUILD) $@
