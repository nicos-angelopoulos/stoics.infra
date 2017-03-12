/** imported_from( +Clauser, +Mod ).

Holds if Goal corresponding to Clauser (a goal or predicate identifier)
hs predicate_property/2 defined property imported_from(Mod), else
Mod is user.

@author nicos angelopoulos
@version  0.1 2017/2/22

*/
imported_from( Pname/Parity, Mod ) :-
    !,
    functor( Goal, Pname, Parity ),
    imported_from( Goal, Mod ).
imported_from( Goal, Mod ) :-
    predicate_property( Goal, imported_from(Mod) ),
    !.
imported_from( _Goal, user ).
