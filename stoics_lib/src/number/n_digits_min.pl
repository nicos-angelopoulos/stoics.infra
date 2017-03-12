/** n_digits_min( +N, +Number, -Padded ).

Padded is the atom coresponding to Number with the possible
addition of leading 0s to pad the length to a minimum of legth = N.

==
 ?- n_digits_min( 2, 2, Atom ).
 Atom = '02'.
==

@see n_digits/3 for a procrustean version

*/

n_digits_min( N, Int, Padded ) :-
	number_codes( Int, Codes ),
	length( Codes, Len ),
	Pad is N - Len,
	findall( 0'0, between(1,Pad,_), Zs ),
	append( Zs, Codes, PaddedCs ),
	atom_codes( Padded, PaddedCs ).
