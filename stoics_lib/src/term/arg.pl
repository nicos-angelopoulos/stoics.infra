
:- lib(nth1/5).

%% arg( ?N, +TermIn, +NewNth, ?Nth, -TermOut ).
% 
%  Find and replace nth arg in a term.
%
%==
% arg( N, row(a,b,c), 3, c, Out ).
%
% N = 3,
% Out = row(a, b, 3) ;
% false.
% 
%==
%
%@author Nicos Angelopoulos
%@version 0.1 2012/06/06
%@see nth1/5
%

arg( N, Tin, New, Old, Tou ) :-
     Tin =.. [Name|Args],
     nth1( N, Args, New, Old, NewArgs ),
     Tou =.. [Name|NewArgs].

/** arg( +N, +TermIn, -Nth, -TermOut ).
	
	Extends arg/3 to an extra argument that returns TermIn without the N position argument.
As of version 0.2 N can also be a list of Ns. 

==
?- arg( 3, a(1,2,3,4), Three, Term ).
Three = 3,
Term = a(1, 2, 4).

?- maplist( arg(2), [t(1,2,3),t(4,5,6),t(7,8,9)], Args, Terms ).
Args = [2, 5, 8],
Terms = [t(1, 3), t(4, 6), t(7, 9)].

?- arg( [1,3], a(x,y,z,w), Nths, Rem ).
Nths = [x, w],
Rem = a(y, z).

==

@author nicos angelopoulos
@version  0.1 2016/6/15
@version  0.2 2018/4/27, first argument can now be a list.
@see nth1/4

*/
arg( List, Tin, Nths, Tout ) :-
    is_list(List),
    !,
    arg_list_nths( List, Tin, Nths, Tout ).
arg( N, Tin, Nth, Tout ) :-
	Tin =.. [Name|Args],
	nth1( N, Args, Nth, RArgs ),
	Tout =.. [Name|RArgs].

arg_list_nths( [], Tout, [], Tout ).
arg_list_nths( [H|T], Tin, [Nth|Nths], Tout ) :-
    arg( H, Tin, Nth, Tmd ),
    arg_list_nths( T, Tmd, Nths, Tout ).
