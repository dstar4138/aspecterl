## Async Server Example ##

The following example shows how to use AspectErl to quickly implement modules
with OTP behaviours. 

OTP behaviours introduce quite a bit of boilerplate that isn't necessary if you
are only utilizing a few features. One example is writing a simple asynchronous
server using the `gen_server` behaviour. Note you really only would want to 
implement `init/1` and `handle_cast/2`, and all other functions should stub out
and work correctly no matter what your state is. 

Wouldn't it be cleaner if you ignored implementing them, and only implemented 
the functions you want and add the following to the top: 
`-aspect(inject_missing).`


### To Build ###

Run `make` which will build everything assuming you are able to pull the 
dependencies. If this doesn't work the first time, do a `make clean` before
rebuilding.

To build without aspects run: `make aspects_off`

Note that building with them off causes an error that certain functions are
not implemented.

