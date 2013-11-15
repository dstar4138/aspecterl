REBAR=../../bin/rebar

compile: deps
	$(REBAR) update-deps compile

clean:
	$(REBAR) clean

aspects_off: deps
	$(REBAR) update-deps compile -DAspectsOn=false

deps:
	$(REBAR) get-deps

distclean: clean
	$(REBAR) delete-deps
	-rmdir deps
	-rmdir ebin
