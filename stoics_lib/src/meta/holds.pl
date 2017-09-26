:- lib(mod_goal/2).

/** holds( +Goal, -Holds ).
    
    Goal is called deterministically with =|Holds = true|= iff Goal
    succeeds. Else, =|Holds = false|=.

    Note that if Holds is instantiated, Goal will still be called,
    with holds/2 succeeding iff Holds corresponds to the right outcome from Goal.

==
?- holds( X=3, Holds ).
X = 3,
Holds = true.

?- holds( 4=3, Holds ).
Holds = false.

?- holds( member(X,[a,b]), Holds ).
X = a,
Holds = true.


?- holds( member(X,[a,b]), non_true ).
false.

?- holds( (write(x),nl), non_true ).
x
false.

?- holds( member(X,[a,b]), false ).
false.
==

@author nicos angelopoulos
@version  0.1 2015/12/9
@version  0.2 2017/9/25, added mod_goal/2

*/
holds( Goal, Holds ) :-
    mod_goal( Goal, Moal ),
    call( Moal ),
    !,
    Holds = true.
holds( _Goal, false ).
