# ------------------------------
# macro
# ------------------------------
ROOT ?= $(PWD)
SRC  :=$(ROOT)/src
DEST :=$(ROOT)/dest
SUMMARY := $(ROOT)/summary.log
BUILD := $(ROOT)/build.log
UNIT := $(ROOT)/unittest.log
INTEGRATE := $(ROOT)/integrate.log
SWF := $(ROOT)/test
INSTALL := $(ROOT)/install.log

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
	(/bin/echo -n "$1 ... "; \
	if test -e $1; then \
	  /bin/echo "ok"; \
	else \
	  /bin/echo "not found"; \
	fi)
endef

# ------------------------------
.PHONY: check build build__ unit unit__ integrate integrate__ install install__ swf swf__ web all code clean

all : code build unittest integrate install swf web twitter

check:
	$(call check_prog,git)
	$(call check_prog,ocaml)
	$(call check_prog,ocamlfind)
	$(call check_prog,omake)
	$(call check_prog,avmplus)
	$(call check_prog,swfmill)

code:
	rm -rf $(SRC)
	git clone --depth 1 git://github.com/mzp/scheme-abc.git $(SRC)

build: check
	$(MAKE) -f $(ROOT)/Makefile --directory=$(SRC) build__ ROOT=$(ROOT)

build__:
	git clean -fx
	omake config PREFIX=$(DEST)
	omake all | tee $(BUILD)

unit__:
	omake check-detail
	mv unittest.log $(UNIT)

unit:
	$(MAKE) -f $(ROOT)/Makefile --directory=$(SRC) unit__ ROOT=$(ROOT)


integrate:
	$(MAKE) -f $(ROOT)/Makefile --directory=$(SRC) integrate__ ROOT=$(ROOT)
integrate__:
	omake integrate-detail
	mv integrate.log $(INTEGRATE)

install:
	cd $(SRC) && omake install
	$(MAKE) -f $(ROOT)/Makefile --directory=$(DEST) install__ ROOT=$(ROOT)

install__:
	$(call check_file,bin/habc) | tee $(INSTALL)
	$(call check_file,bin/habc-scm) | tee -a $(INSTALL)
	$(call check_file,bin/habc-xml) | tee -a $(INSTALL)
	$(call check_file,lib/habc/std.ho) | tee -a $(INSTALL)
	$(call check_file,lib/habc/stub.stub.ho) | tee -a $(INSTALL)
	$(call check_file,lib/habc/flash.stub.ho) | tee -a $(INSTALL)
	$(call check_file,share/habc/template.xml) | tee -a $(INSTALL)
	$(call check_file,share/habc/example) | tee -a $(INSTALL)

swf:
	$(MAKE) -f $(ROOT)/Makefile --directory=$(DEST)/share/habc/example swf__ ROOT=$(ROOT)

swf__:
	$(DEST)/bin/habc binaryClock.scm -o $(SWF)

web:
	ruby makeweb.rb > /var/www/vhosts/happyabc/htdocs/snapshot.html

twitter:
	grep -i 'fail\|error' *.log && twtr up -m '@mzp hey! Something wrong! see http://happyabc.org/snapshot.html'

clean:
	rm -rf $(SRC)
	rm -rf $(DEST)
	rm -f *.log *.swf