
:- use_module( library(options) ).

ex_sort_defaults( Defs ) :-
	Defs = [     duplicates(false),
	             order(<)               ].

/** ex_sort( +List, -Ordered, +Opts ).

An artificial example to illustrate the use of library(options).

The example adds some options to the standard predicate sort/3.

Options
  * duplicates(Dups=false)
    whether to allow duplicates
  * debug(Dbg=false)
    turn debug(ex_sort) on for the call
  * order(Ord=<)
    the order of the sort

==
?- ex_sort( [a,b,e,c,b], Ord, true ).
Ord = [a, b, c, e].
==

Identical to sort( List, Ord ).

==
?- ex_sort( [a,b,e,c,b], Ord, debug(true) ).
% Input list length: 5
% Output list length: 4
Ord = [a, b, c, e].
==
Turn debugging on for the call.

==
?- ex_sort( [a,b,e,c,b], Ord, true ).
Ord = [a, b, c, e].
==

Debug for predicate returns to previous status.

==
?- ex_sort( [a,b,e,c,b], Ord, duplicates(true) ).
Ord = [a, b, b, c, e].
==
Predicate has option for returning duplicates.

==
?- ex_sort( [a,b,e,c,b], Ord, order(>) ).
Ord = [e, c, b, a].
==

*/
ex_sort( List, Ordered, Args ) :-
	Self = ex_sort,
	options_append( Self, Args, Opts ),
	length( List, ListLen ),
	debug( ex_sort, 'Input list length: ~w', ListLen ),
	sort( List, SortOrder ),
	options( order(Ord), Opts ),
	ex_sort_order( Ord, SortOrder, DirOrdered ),
	options( duplicates(Dup), Opts ),
	options_example_duplicates( Dup, DirOrdered, List, Ordered ),
	length( Ordered, OrderedLen ),
	debug( Self, 'Output list length: ~w', OrderedLen ),
	options_restore( Self, Opts ).

ex_sort_order( <, Ordered, Ordered ).
ex_sort_order( >, List, Ordered ) :-
	reverse( List, Ordered ).

options_example_duplicates( true, Dired, List, Ordered ) :-
	findall( D, (member(D,Dired),member(D,List)), Ordered ).
options_example_duplicates( false, Dired, _List, Dired ).
