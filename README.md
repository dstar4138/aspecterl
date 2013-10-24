# AspectErl #

The following is a graduate-project for the Aspect-Oriented Programming course 
under Professor James Heliotis at The Rochester Institute of Technology.

## Description ##

AspectErl is a compiler tool that can be inserted into your compile-time 
pipeline via [rebar](https://github.com/rebar/rebar). It is capable of weaving 
code (advice) via the pre-processor using pointcuts and decorator annotations.
This can be useful for adding monitoring capabilities or building process pools
without explicitly modifying the source code.

Note: This implementation does not to AspectJ's syntax where it would be 
un-natural in the Erlang Language.

## License ##

AspectErl uses portions of 
[ErlAOP](http://erlaop.sourceforge.net/) 
maintained and copyrighted by 
[Alexei Krasnopolski](http://sourceforge.net/users/krasnopolski) 
and licensed under 
[Apache License V2](http://sourceforge.net/directory/license:apache2/).
AspectErl is MIT licensed.

## Contributing ##

If you find problems with AspectJ, please submit a test case along with your
issues. Or if you are so inclined, fork AspectErl and send back a pull request!
 
