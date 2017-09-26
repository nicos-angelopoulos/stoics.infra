
:- lib(mod_goal/2).
:- lib(message_report/3).

/** maparg( +Pname, ?Term1 ).
    maparg( +Pname, ?Term1, ?Term2 ).
    maparg( +Pname, +Npos, ?Term1, ?Term2 ).

Call Pname on all paired Term1 and Term2 arguments.
When Npos is present it should be an integer I: [-1,0,1,2]. -1 stands for not inclusions of the 
argument (default).  Npos is the position at which the index of the argument can be added to the call/3.

==
?- maparg( number, row(1,2,3) ).
true.

?- assert( times(X,Y,Product) :- Product is X * Y).
?- maparg( times(2), c(1,2,3), Term ).
Term = c(2, 4, 6).

?- assert( times3(X,Y,Z,Product) :- Product is X * Y * Z).
?- maparg( times3(2), 1, c(1,2,3), Term ).
Term = c(2, 8, 18).

?- maparg( times(2), -1, c(1,2,3), Term ).
Term = c(2, 4, 6).
==

The last example adds indices: 1, 2 and 3 to the 3 calls to times3, thus the call can 
be informed of the positional context of the element.

@author nicos angelopoulos
@version  0.2 2014/3/5,  added Npos
@version  0.3 2014/4/3,  added maparg/2
@version  0.4 2017/9/25, pass meta-goals through mod_goal/2

*/
maparg( Pname, Term ) :-
    functor( Term, _Tname, Tarity ),
    mod_goal( Pname, Mname ),
    maparg_2( Tarity, Mname, Term ).

maparg( Pname, Goal1, Goal2 ) :-
    functor( Goal1, Gname, Garity ),
    functor( Goal2, Gname, Garity ),
    % functor( Call, Pname, 2 ),
    mod_goal( Pname, Mname ),
    maparg_1( Garity, Mname, -1, Goal1, Goal2 ).

maparg( _Pname, Npos, _Goal1, _Goal2 ) :-
    ( Npos < -1; Npos > 2 ),
    !,
    Format = 'maparg/4: Npos should be integer in -1=<x=<2, found:~w',
    Args = Npos,
    message_report( Format, Args, error ),
    fail.
maparg( Pname, Npos, Goal1, Goal2 ) :-
    functor( Goal1, Gname, Garity ),
    functor( Goal2, Gname, Garity ),
    mod_goal( Pname, Mname ),
    maparg_1( Garity, Mname, Npos, Goal1, Goal2 ).

maparg_2( 0, _Goal, _Term ) :- !.
maparg_2( I, Goal, Term ) :-
    arg( I, Term, Arg ),
    call( Goal, Arg ),
    H is I - 1,
    maparg_2( H, Goal, Term ).

maparg_1( 0, _Call, _Npos, _Goal1, _Goal2 ) :- !.
maparg_1( I, CallE, Npos, Goal1, Goal2 ) :-
    copy_term( CallE, Call ),
    arg( I, Goal1, Arg1 ),
    % arg( 1, Call, Arg1 ),
    % arg( 2, Call, Arg2 ),
    maparg_call( Npos, I, Call, Arg1, Arg2 ),
    arg( I, Goal2, Arg2 ),
    K is I - 1,
    maparg_1( K, CallE, Npos, Goal1, Goal2 ).

maparg_call( -1, _I, Call, Arg1, Arg2 ) :-
    call( Call, Arg1, Arg2 ).
maparg_call( 0, I, Call, Arg1, Arg2 ) :-
    call( Call, I, Arg1, Arg2 ).
maparg_call( 1, I, Call, Arg1, Arg2 ) :-
    call( Call, Arg1, I, Arg2 ).
maparg_call( 2, I, Call, Arg1, Arg2 ) :-
    call( Call, Arg1, Arg2, I ).
