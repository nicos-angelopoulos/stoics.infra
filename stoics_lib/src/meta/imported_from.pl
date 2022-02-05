/** imported_from( +Clauser, +Mod ).

Holds if Goal corresponding to Clauser (a goal or predicate identifier)
and predicate_property/2 defines property imported_from(Mod), else Mod is user.

@author nicos angelopoulos
@version  0.1 2017/2/22
@version  0.2 2022/2/05,  try user:Goal if failed on G

*/
imported_from( Pname/Parity, Mod ) :-
    !,
    functor( Goal, Pname, Parity ),
    imported_from( Goal, Mod ).
imported_from( Goal, Mod ) :-
    predicate_property( Goal, imported_from(Mod) ),
    !.
imported_from( Goal, Mod ) :-
    predicate_property( user:Goal, imported_from(Mod) ),
    !.
imported_from( _Goal, user ).
