## TODO ##

While the primary purpose of AspectErl is finished, there are a few things 
that would be nice to finish up before calling this a finished project:

* Shell based compilation. It is avaliable in ErlAOP, to use ADF files, since
  AspectErl doesn't use them much I put off implementing it.

* ADF based injection. I implemented my own parser based on ErlAOP's ADF idea,
  but haven't put that into the pipeline.

* Documentation. I would like to clean up the code and document it so it serves
  to be more than just a half-baked idea.

* Fix all the TODOs in the source code.

* If two pointcuts are named the same in two different modules, they get applied
  twice. Perhaps pointcuts should be modularized?
