## Server Pool Example ##

The following example shows how to use AspectErl to make particular servers 
members of a server pool without modifying the servers themselves.

Note there are four server modules, two Servers devoted to hypothetical task 
'a' and two for task 'b'. We then use the advice module, `pool_advice`, to force
the servers of task 'b' into a Process Group. This could theoretically be used
to patch in a separate module or application that communicates with a 
particular process group.

### To Build ###

Run `make` which will build everything assuming you are able to pull the 
dependencies. If this doesn't work the first time, do a `make clean` before
rebuilding.

To build without aspects run: `make aspects_off`


### To Run ###

Run the following commands:

    cd ebin
    erl -eval 'pooler:start().'

This will drop you into the Erlang shell and start the application running. 
You can verify which processes have been put into the process group by running:

    > pooler:checkpg().

This will return the Process IDs of the processes in the group. If nothing has 
been added, then you will get a raised exception stating that no such group 
exists (badarg,bServer). 

To quit:

    > pooler:stop().
