/** imported_from( +Clauser, +Mod ).

Holds if Goal corresponding to Clauser (a goal or predicate identifier)
and predicate_property/2 defines property imported_from(Mod).

Up to v0.2 this used to succeeed with ==Mod=user== if Clauser was not imported from anywhere.

@author nicos angelopoulos
@version  0.1 2017/02/22
@version  0.2 2022/02/05, try user:Goal if failed on G
@version  0.3 2022/11/19, remove success to user when not imported

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
% imported_from( _Goal, user ).  % removed at v0.3
