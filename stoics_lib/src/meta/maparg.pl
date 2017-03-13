/** maparg( +Pname, ?Term1 ).
maparg( +Pname, ?Term1, ?Term2 ).
maparg( +Pname, +Npos, ?Term1, ?Term2 ).

Call Pname on all paired Term1 and Term2 arguments.
When Npos is present it should be an integer I, -1 =< 1 =< 2. -1 stands for not inclusions of the 
argument (default).  Npos is the position at which the location of the argument can be added to the call/3.

==
?- maparg( number, row(1,2,3) ).
true.

?- assert( times(X,Y,Product) :- Product is X * Y).
?- maparg( times(2), c(1,2,3), Term ).
Term = c(2, 4, 6).

?- assert( times3(X,Y,Z,Product) :- Product is X * Y * Z).
?- maparg( times3(2), 1, c(1,2,3), Term ).
Term = c(2, 8, 18).
==

The last example adds indices: 1, 2 and 3 to the 3 calls to times3, thus the call can 
be informed of the positional context of the element.

@author nicos angelopoulos
@version  0.2 2014/3/5, added Npos
@version  0.3 2014/4/3, added maparg/2
*/
maparg( Pname, Term ) :-
     functor( Term, _Tname, Tarity ),
     maparg_2( Tarity, Pname, Term ).

maparg( Pname, Goal1, Goal2 ) :-
     functor( Goal1, Gname, Garity ),
     functor( Goal2, Gname, Garity ),
     % functor( Call, Pname, 2 ),
     maparg_1( Garity, Pname, -1, Goal1, Goal2 ).

maparg( _Pname, Npos, _Goal1, _Goal2 ) :-
	( Npos < 0; Npos > 2 ),
	!,
	Format = 'maparg/4: Npos should be integer in 0=<x=<2, found:~w',
	Args = Npos,
	message_report( Format, Args, error ),
	fail.
maparg( Pname, Npos, Goal1, Goal2 ) :-
     functor( Goal1, Gname, Garity ),
     functor( Goal2, Gname, Garity ),
     maparg_1( Garity, Pname, Npos, Goal1, Goal2 ).

maparg_2( 0, _Pname, _Term ) :- !.
maparg_2( I, Pname, Term ) :-
     arg( I, Term, Arg ),
	call( Pname, Arg ),
	H is I - 1,
	maparg_2( H, Pname, Term ).

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
