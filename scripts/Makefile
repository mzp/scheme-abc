# ------------------------------
# macro
# ------------------------------
ROOT :=$(PWD)
SRC  :=$(ROOT)/src
DEST :=$(ROOT)/dest
SUMMARY := $(ROOT)/summary.log
BUILD := $(ROOT)/build.log
UNIT := $(ROOT)/unittent.log
INTEGRATE := $(ROOT)/intergate.log

define check_prog
	@/bin/echo -n "checking $1 ... "
	@if which $1 > /dev/null; then \
	  /bin/echo "ok"; \
	 else \
	  /bin/echo "not found"; \
	  exit 1; \
	 fi
endef

define check_file
	/bin/echo -n "checking $1 ... "; \
	if test -e $1; then \
	  /bin/echo "ok"; \
	else \
	  /bin/echo "not found"; \
	  exit 1; \
	fi
endef

# ------------------------------
.PHONY: check build unit integrate swf web all code clean

all : web

check:
	$(call check_prog,git)
	$(call check_prog,ocaml)
	$(call check_prog,ocamlfind)
	$(call check_prog,omake)
	$(call check_prog,avmplus)
	$(call check_prog,swfmill)

code: $(SRC)

$(SRC):
	rm -rf $(SRC)
	git clone --depth 1 git://github.com/mzp/scheme-abc.git $(SRC)

build: check $(SRC)
	cd $(SRC) && \
	git clean -fx && \
	omake config PREFIX=$(DEST) && \
	omake all | tee $(BUILD)

unit: build
	cd $(SRC) && \
	omake check-detail && \
	mv unittest.log $(UNIT)

integrate: build
	cd $(SRC) && \
	omake integrate && \
	mv integrate.log $(INTEGRATE)

swf: check
	cd $(SRC) && \
	omake install && \
	$(call check_file,$(DEST)/bin/habc) | tee swf.log ; \
	$(call check_file,$(DEST)/bin/habc-scm) | tee swf.log ; \
	$(call check_file,$(DEST)/bin/habc-xml) | tee swf.log ; \
	$(call check_file,$(DEST)/lib/habc/std.ho) | tee swf.log ; \
	$(call check_file,$(DEST)/lib/habc/stub.stub.ho) | tee swf.log ; \
	$(call check_file,$(DEST)/lib/habc/flash.stub.ho) | tee swf.log; \
	$(call check_file,$(DEST)/share/template.xml) | tee swf.log; \
	$(call check_file,$(DEST)/share/example) | tee swf.log

web: unit integrate swf

clean:
	rm -rf $(SRC)
	rm -rf $(DEST)
	rm -f *.log