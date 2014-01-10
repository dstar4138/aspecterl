## TODO ##

While the primary purpose of AspectErl is finished, there are a few things
that would be nice to add:

* Mockup injection - Replacing the `inject_missing` with a mock-up repo, which
  would allow for a more general injection mechanism.

* Inlining - Right now nothing is optimized, it would be interesting to see if
  optimizations can be made to the injection of before and after advice to 
  reduce remote calls.

* A Rebar plugin to ease ADF compiles.

* A Testing framework using eunit would be a requirement for use in production.
  This hasn't been a priority since none of my projects utilize it.

