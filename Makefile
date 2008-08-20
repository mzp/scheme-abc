OCAMLBUILD:=ocamlbuild
TestList:=$(wildcard test/test_*.ml)

byte: generate
	$(OCAMLBUILD) main.byte

## test
all-test: unit integrate

test: unit

unit: generate test/list camlp4/TestCaseCollector.cmo
	rm -f _build/src/asm.cm[oi]
	rm -f _build/test/runner.cm[oi]
	$(OCAMLBUILD) runner.byte --

test/list: $(TestList)
	for i in $^; do\
    echo open $$(basename $$i .ml); \
  done | sed 's/open test/open Test/' > $@

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
.PHONY: clean count count-all count-src
clean:
	$(OCAMLBUILD) -clean
	rm -f  *~ */*~ *.abc *.cm[io] */*.cm[io] src/match.ml src/opcode.{ml,mli}

doc:
	for i in src/*.mli ; do \
	  basename $$i .mli; \
	done | ruby -e "ARGF.each{|x| puts x.capitalize}"> scheme-abc.odocl
	ocamlbuild scheme-abc.docdir/index.html

count:
	wc -l src/*.{ml,mli} | tail -1
	wc -l {src,test,util}/*.ml src/*.mli example/*.scm | tail -1

count-all:
	wc -l {src,test,util}/*.ml src/*.mli example/*.scm

count-src:
	wc -l src/*.{ml,mli}

## general rules
%.cmo %.byte::
	$(OCAMLBUILD) $@

%.abc : example/%.scm byte
	$(OCAMLBUILD) main.byte -- -o$@ $<
	avmplus $@
