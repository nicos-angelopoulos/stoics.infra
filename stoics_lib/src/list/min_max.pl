/** min_max( +List, -Min, -Max ).

Find the minimum and the maximum elements of a list of numbers in one pass.

==
?- numlist(1,4,ToFour), min_max(ToFour,Min,Max).
Min = 1,
Max = 4.
==

@author nicos angelopoulos
@version  0.1 2014/5/7
@version  0.2 2018/2/16,   was max_min_list/3 (with switched outputs)

*/
min_max( [H|T], Min, Max ) :-
	min_max_1( T, H, H, Min, Max ).

min_max_1( [], Min, Max, Min, Max ).
min_max_1( [H|T], CurMin, CurMax, Min, Max ) :-
	( H > CurMax ->
		NxMax is H,
		NxMin is CurMin
		;
		NxMax is CurMax,
		( H < CurMin ->
			NxMin is H 
			;
			NxMin is CurMin
		)
	),
	min_max_1( T, NxMin, NxMax, Min, Max ).
