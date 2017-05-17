/** op_compare( ?Op, +Term1, +Term2 ).

Extends compare/3 on ground operators with all known operators. Also recognizes Op = (>:<) 
(ground only), which always succeeds.

==
?- op_compare( =<, 2, 3 ).
true.

?- op_compare( Op, 2, 3 ).
Op =  (<).

?- op_compare( >:<, 2, 3 ).

==

*/
op_compare( Op, Term1, Term2 ) :-
	ground( Op ),
	!,
	op_compare_ground( Op, Term1, Term2 ).
op_compare( Op, Term1, Term2 ) :-
	compare( Op, Term1, Term2 ).

op_compare_ground( >:<, _Term1, _Term2 ) :- !.
op_compare_ground( Op, Term1, Term2 ) :-
	Goal =.. [Op,Term1,Term2],
	call( Goal ).
