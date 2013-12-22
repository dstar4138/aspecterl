REBAR=../../bin/rebar
ERL=$(shell which erl)

# Directories for compiling from ADF.
SRC=$(CURDIR)/src
ADV=$(CURDIR)/advice
EBIN=$(CURDIR)/ebin

.PHONY: compile aspects_off clean distclean 

# Note secondary compile step to inject advice.
# This is not strictly neccessary, just lets rebar compile deps for us.
compile: aspects_off
	$(ERL) -pa deps/*/ebin/ -noshell \
		   -run aspecterl compile $(SRC) $(ADV) $(EBIN) -run init stop

aspects_off: deps 
	$(REBAR) update-deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps
	-rmdir deps
	-rmdir ebin

