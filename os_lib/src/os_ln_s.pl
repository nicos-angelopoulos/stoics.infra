/** os_ln_s( From, To ).

Symbolic link file From to location To.

*/
os_ln_s( From, To ) :-
    maplist( os_cast(atom), [From,To], [FromA,ToA] ),
	link_file( FromA, ToA, symbolic ).
