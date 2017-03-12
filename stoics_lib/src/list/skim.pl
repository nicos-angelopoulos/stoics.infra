
/** skim( +Nested, -Scum, -Remains ).

	Skim the first elements (Scum) from a Nested list with the tails 
	being the Remains.
*/
skim([], [], [] ).
skim([[H|T]|M], [H|RScum], [T|R] ) :-
     skim(M, RScum, R).
