REBAR=../../bin/rebar

.PHONY: compile aspects_off clean distclean

compile: deps
	$(REBAR) update-deps compile

clean:
	$(REBAR) clean

aspects_off: deps
	$(REBAR) -Dnoaspects update-deps compile

deps:
	$(REBAR) get-deps

distclean: clean
	$(REBAR) delete-deps
	-rmdir deps
	-rmdir ebin
