
:- lib(nth1/5).
:- lib(compound/3).

%% arg( ?N, +TermIn, +NewNth, ?Nth, -TermOut ).
% 
%  Find and replace nth arg in a term.
%
%==
% ?- arg( 3, row(a,b,c), x, OldArg, Out ).
% OldArg = c,
% Out = row(a, b, x).
%
% ?- arg( N, row(a,b,c), x, c, Out ).
% N = 3,
% Out = row(a, b, x) ;
% false.
%==
%
%@author Nicos Angelopoulos
%@version  0.1 2012/6/6
%@version  0.2 2019/1/8,  use compound/3
%@see nth1/5
%

arg( N, Tin, New, Old, Tou ) :-
     compound( Tin, Name, Args ),
     nth1( N, Args, New, Old, NewArgs ),
     compound( Tou, Name, NewArgs ).

/** arg( +N, +TermIn, -Nth, -TermOut ).
	
	Extends arg/3 to an extra argument that returns TermIn without the N position argument.

As of version 0.2 N can also be a list of Ns. The list will first be sorted, and got rid off duplicates,
before applied to finding the positions. 

As of version 0.3 compound/3 instead of =.. is used.

==
?- arg( 3, a(1,2,3,4), Three, Term ).
Three = 3,
Term = a(1, 2, 4).

?- maplist( arg(2), [t(1,2,3),t(4,5,6),t(7,8,9)], Args, Terms ).
Args = [2, 5, 8],
Terms = [t(1, 3), t(4, 6), t(7, 9)].

?- arg( [1,3], a(x,y,z,w), Nths, Rem ).
Nths = [x, z],
Rem = a(y, w).

?- arg( [1,3,2,1], a(x,y,z,w), Nths, Rem ).
Nths = [x, z, y, x],
Rem = a(w).

==

@author nicos angelopoulos
@version  0.1 2016/6/15
@version  0.2 2018/4/27, first argument can now be a list.
@see nth1/4

*/
arg( List, Tin, Nths, Tout ) :-
    is_list(List),
    sort( 0, @>, List, Set ),
    !,
    arg_set_nths( Set, Tin, NthPrs, Tout ),
    findall( Nth, (member(Elem,List),memberchk(Elem-Nth,NthPrs)), Nths ).
arg( N, Tin, Nth, Tout ) :-
    compound( Tin, Name, Args ),
	nth1( N, Args, Nth, RArgs ),
    compound( Tout, Name, RArgs ).

arg_set_nths( [], Tout, [], Tout ).
arg_set_nths( [H|T], Tin, [H-Nth|Nths], Tout ) :-
    arg( H, Tin, Nth, Tmd ),
    arg_set_nths( T, Tmd, Nths, Tout ).
