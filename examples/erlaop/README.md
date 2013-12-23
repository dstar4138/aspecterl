## ErlAOP ADF Example ##

This example demonstrates the use of ADF configurations and how to utilize 
AspectErl's ADF parser/compiler. For more information on ADF syntax, please
consult the wiki.


### To Build ###

Run `make` which will build everything assuming you are able to pull the 
dependencies. If this doesn't work the first time, do a `make clean` before
rebuilding.

To build without aspects run: `make aspects_off`

Note you will need to `make clean`, if you already made it with aspects on. 
This is because rebar and Erlang will think nothing's changed and won't really
compile anything.

### To Run ###

Run the following commands:

    cd ebin
    erl -eval 'tester:run().'

This will drop you into the Erlang shell and run the testing function. If the
aspects have been injected correctly, you will see a `Entering` and `Exiting` 
messages which have been added by the advice.

To quit:

    > q().

This will merely close the Erlang shell. Try rebuilding without aspects and 
run it all again too.

