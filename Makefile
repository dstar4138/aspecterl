REBAR=./bin/rebar

compile: deps
	$(REBAR) update-deps compile

examples:
	cd examples; make all

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps
	-rmdir deps
	-rmdir ebin
	cd examples; make distclean

#TODO: Testing, eunit, etc.

deps:
	$(REBAR) get-deps
