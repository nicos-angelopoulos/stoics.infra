/** os_ln_s( From, To ).

Symbolic link file From to location To.

*/
os_ln_s( From, To ) :-
	os_cast( From, atom, FromA ),
	os_cast( To, atom, ToA ),
	link_file( FromA, ToA, symbolic ).
