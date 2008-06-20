OCAMLBUILD:=ocamlbuild

byte: generate
	$(OCAMLBUILD) main.byte

## test
test: unit

unit: generate camlp4/TestCaseCollector.cmo
	rm -f _build/src/asm.cm[oi]
	$(OCAMLBUILD) runner.byte --

integrate:
	sh example/test.sh example/*.scm

## auto generate files
generate: src/match.ml src/opcode.ml

src/match.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -m < util/instruction.txt > $@
	$(OCAMLBUILD) -quiet util/instruction.byte -- -s < util/instruction.txt >> $@

src/opcode.ml : util/instruction.txt
	$(OCAMLBUILD) -quiet util/instruction.byte -- -t < util/instruction.txt > $@

## pasued target
.PHONY: clean count count-src
clean:
	ocamlbuild -clean
	rm -f  *~ */*~ *.abc *.cm[io] */*.cm[io] src/match.ml src/opcode.{ml,mli}

count:
	wc -l {src,test,util}/*.ml src/*.mli example/*.scm

count-src:
	wc -l src/*.{ml,mli}

## general rules
%.cmo %.byte::
	$(OCAMLBUILD) $@
