
:- use_module(library(lists)).  % append/3.

:- lib(lexi/2).
:- lib(break_nth/4).

/** lexi_n( +InLexi, +N, +PadC, -Lexi ).

Lexi is of length N lexical object containing either the last N codes of InLexi
or all of InLexi left-padded by PadCs, to make its codes representation
up to length N. By default Lexi is returned as a list of codes.

==
?- lexi_n( `2`, 3, 0'0, Codes ), atom_codes( Atom, Codes ).
Codes = [48, 48, 50],
Atom = '002'.

?- lexi_n( `2`, 3, 0'0, + Atom ).
Atom = '002'.

?- lexi_n( `text`, 8, 0' , Codes ), atom_codes( Atom, Codes ).
Codes = [32, 32, 32, 32, 116, 101, 120, 116],
Atom = '    text'.

?- lexi_n( `text`, 8, 0' , + Atom ).
Atom = '    text'.

==

@author nicos angelopoulos
@version  0.1 2014/03/17
@version  0.2 2022/11/06, this used to be codes_n_digits/3.
@see lexi/2
@see n_digits_integer_codes/3

*/
lexi_n( ILexi, N, PadC, OLexi ) :-
     lexi( ILexi, -(ICodes) ),
	length( ICodes, ILen ),
	Pad is max(N-ILen,0),
	findall( PadC, between(1,Pad,_), PadL ),
	Del is max(ILen-N,0),
	break_nth( Del, ICodes, _DCodes, KCodes ),
	append( PadL, KCodes, Codes ),
     lexi( Codes, OLexi ).
