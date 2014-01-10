# AspectErl #

The following is a graduate-project for the Aspect-Oriented Programming course 
under Professor James Heliotis at The Rochester Institute of Technology. It 
should be seen as a proof-of-concept and is not recommended to be used in 
production software.


## Description ##

AspectErl is a compiler tool that can be inserted into your compile-time 
pipeline via [rebar](https://github.com/rebar/rebar). It is capable of weaving 
code (advice) via the pre-processor using pointcuts and advice decorator 
annotations. This can be useful for adding monitoring capabilities or building 
process pools without explicitly modifying the source code.

AspectErl extends [ErlAOP](http://erlaop.sourceforge.net/) in that it provides 
an
[Annotation API](https://github.com/dstar4138/aspecterl/wiki/Annotation-API) 
and rebar support on top of supporting their 
[ADF configuration file](https://github.com/dstar4138/aspecterl/wiki/Advice-Definition-Files)
syntax. 

AspectErl also adds more capability for selecting modules and functions using 
pointcuts by adding behaviour checking. In otherwords, you can inject code into
all `gen_server` modules. Or by using the `-aspecterl( [inject_missing] ).` 
annotation flag, you can avoid having to implement unneeded functions. 

Note: This implementation does not stick to AspectJ's syntax where it would be 
un-natural in the Erlang Language.


## How to use AspectErl in your Project ##

Add AspectErl as a dependency and library in your Rebar configuration file, then
add the injector as a compiler option:


```
{deps, [ 
    {aspecterl, ".*", {git, "git://github.com/dstar4138/aspecterl"}}
]}.

{erl_opts, [ {parse_transform, aspecterl} ]}.

```

Then write your advice/pointcuts in normal Erlang. Using the annotation format,
these advice files can right along-side your code in the `src/` directory. Then
tell Rebar to compile them first:


```
{erl_first_files, ["src/example_advice.erl"]}.
```

If you want to use the ADF configuration files method (similar to AspectJ 
Schema-based config files) you will need to specify a separate directory where 
AspectErl will find your advice. Currently ADF compilation needs a secondary
step (which in the erlaop example facilitates with a Makefile step). One of my 
todos is a rebar plugin which will remove this requirement.
 
## License ##

AspectErl got hints from
[ErlAOP](http://erlaop.sourceforge.net/) 
maintained and copyrighted by 
[Alexei Krasnopolski](http://sourceforge.net/users/krasnopolski) 
and licensed under 
[Apache License V2](http://sourceforge.net/directory/license:apache2/).
AspectErl is MIT licensed.


## TODO ##

There are a couple missing pieces that I would like to implement as time allows,
please see the 
[TODO.md](https://github.com/dstar4138/aspecterl/blob/master/TODO.md)
document for more information.


## Contributing ##

If you find problems with AspectErl, please submit a test case along with your
issues. Or if you are so inclined, fork AspectErl and send back a pull request!

