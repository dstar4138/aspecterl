REBAR=./bin/rebar

compile: deps
	$(REBAR) update-deps compile

clean:
	$(REBAR) clean

distclean:
	$(REBAR) delete-deps clean

#TODO: Testing, eunit, etc.

deps:
	$(REBAR) get-deps
