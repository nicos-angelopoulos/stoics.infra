
:- use_module(library(lib)).

:- lib(options).
:- lib(debug_call).

list_is_invariant_defaults([]).

/** list_is_invariant(+List).

True iff List's all elements comprise of a single element.

Note that the empty list is not an invariant list according to this predicate.

Opts
  * debug(Dbg=false)
    informational, progress messages

Examples
==
?- list_is_invariant([1,1,1]).

?- list_is_invariant([]).
false.

?- list_is_invariant([1,2]).
false.

==

@author nicos angelopoulos
@version  0.1 2024/03/24

*/
list_is_invariant( List ) :-
     sort( List, [_] ).
