## Call Logger Example ##

The following example shows how to use AspectErl to dynamically inject logging
into your program as a simple secondary advice file.

There are many different ways but this module gives an example of two:

* Call logging, for each call the system will use `io:fwrite` for printing the 
  arguments that were passed into every function call. Granted this isn't
  super but it gets the point across, and why someone would want to use
  `before` advice.

* Error Logging, all exceptions, even those which may be handled are sent to 
  `io:fwrite`. This will demo why someone may want to use `after_throw` advice.

* Return logging, could be useful when checking the return value of particular
  functions you are having issues debugging? Will demo why someone would want
  to use `after_final` or `after_return` advice.

To toggle these advice on and off check the rebar.config for details.

### To Build ###

Run `make` which will build everything assuming you are able to pull the 
dependencies. If this doesn't work the first time, do a `make clean` before
rebuilding.

To build without aspects run: `make aspects_off`

Note that building with them off removes all logging advice.

