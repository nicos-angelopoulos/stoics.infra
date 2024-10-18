:- module( options, [ 
              options/2, 
              options/3,
              options_append/3, 
              options_append/4,
              options_call/2, options_call/3,
              options_debug_topic/3,
              options_debug/3, 
              options_propagate/4,
              options_rename/4,
              options_restore/2,
              options_return/2,
              options_return/3,
              options_version/2
            ] ).

/**  <module> Options handling and processing.

This is a stoics.infrastructure pack for handling option arguments.
The main concept is to treat options as naive Prolog lists which the
programmer can manipulate and specialise if they need to, while providing a
small number of predicates that manage basic common operations on options.
Options are particularly important in the context of SWI packs, as making
code publicly available to others often involves allowing for variations
in the behaviour of the code.

The library provides simple extensions to the basic list manipulation
predicates. In many cases it is just the error handling that is the main
difference to standard predicates.

Technically the library is designed on the semantics of memberchk/2. 
Looking for an Option in a list of options, memberchk/2 will return the
leftmost match. Library(options) sees options as a concatenation (append/3)
of the user provided options (arguments for hereon) and the defaults
provided by the predicate.

The default option values for a predicate are given by a predicate
of the same name but postfixed by '_defaults'. The library also allows for
reading user specific default options by reading profiles from a file
located at $HOME/.pl/<pred_name>.pl, if that file exists. Each options file
should have a number of option terms given as facts.

Some distinctive features of pack(options)

  * minimal "magic" behind the scenes

  * allows sloppy (un-listed) single option argument

  * defaults might depend on input

  * can-do cascading  (options of distinct predicates should be disjoint)

  * processing debug(Dbg) terms which optionise calls to debug/1

  * uniform access to user specific file-based default options

For an example see program options_example_sort_defaults.pl in examples directory.

==
?- edit( pack(options/examples/ex_sort) ).

?- [pack(options/examples/ex_sort)].

?- ex_sort( [a,b,e,c,b], Ord, true ).
Ord = [a, b, c, e].
?- ex_sort( [a,b,e,c,b], Ord, debug(true) ).
% Input list length: 5
% Output list length: 4
Ord = [a, b, c, e].
?- ex_sort( [a,b,e,c,b], Ord, order(>) ).
Ord = [e, c, b, a].
?- ex_sort( [a,b,e,c,b], Ord, duplicates(true) ).
Ord = [a, b, b, c, e].
==

Create file $HOME/.pl/ex_sort.pl with content
order(>). 

==
?- ex_sort( [a,b,e,c,b], Ord, true ).
Ord = [e, c, b, a].
==
Default for user is now order(>) which can still be over-ridden at invocation
==
?- ex_sort( [a,b,e,c,b], Ord, order(<) ).
Ord = [a, b, c, e].
==

---++ Pack info

Predicates

  * options/2,3
  * options_append/3,4
  * options_propagate/4
  * options_rename/4
  * options_restore/2
  * options_return/2

Thanks to Jan Wielemaker for fixing an issue with expanding the $HOME variable and 
missing curly brackets in the errors DCG (2016/11/14).

@author   nicos angelopoulos
@version  0.2.0 2015/7/5
@version  0.4.0 2016/2/29
@version  0.5.0 2017/3/10
@version  1.0   2018/3/18
@version  1.1   2018/4/8
@version  1.2   2019/4/18
@version  1.3   2020/9/18
@version  1.4   2021/1/22
@version  1.5   2022/12/29
@see http://www.stoics.org.uk/~nicos/sware/options

*/

:- use_module(library(lists)).  % append/3, select/3,...
:- use_module(library(apply)).  % partition/4...
:- use_module(library(debug)).  % /3.
:- use_module(library(filesex)).% directory_file_path/3,...

:- use_module(library(lib)).
:- lib(source(options), homonyms(true)).
        % this shouldn't be necessary but SWIPL (at least 7.3.31-9)
        % "looses" the prolog_load_context( module, _ ) value when 
        % requires/1 is used from auxilary files within the pack

:- lib(suggests(pack_errors)).

:- lib(options/2).
:- lib(options/3).
:- lib(options_auxils/0).
:- lib(options_append/3).
:- lib(options_append/4).
:- lib(options_debug/3).
:- lib(options_propagate/4).
:- lib(options_restore/2).
:- lib(options_return/2).
:- lib(options_return/3).
:- lib(options_call/2).
:- lib(options_call/3).
:- lib(options_errors/0).
:- lib(options_debug_topic/3).
:- lib(options_rename/4).
:- lib(options_version/2).
:- lib(end(options)).
