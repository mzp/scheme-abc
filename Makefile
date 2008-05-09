lib:=extlib
OCAMLC:=ocamlfind ocamlc -linkpkg -package $(lib)
OCAMLBUILD:=ocamlbuild -ocamlc '$(OCAMLC)'

byte:
	$(OCAMLBUILD) main.byte

clean:
	rm -f  *~ */*~
	ocamlbuild -clean