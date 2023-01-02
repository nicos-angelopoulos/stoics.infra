
:- use_module(library(lists)).  % append/3.

:- lib(lexi/2).
:- lib(break_nth/4).

/** lexi_n( +InLexi, +N, ?PadC, -Lexi ).

Lexi is of length N lexical object containing either the last N codes of InLexi
or all of InLexi left-padded by PadCs, to make its codes representation
up to length N. 

By default Lexi is returned as a list of codes but the result can be term shaped, 
as per lexi/2.

PadC will not be touched if length(InLexi) >= N. It should be a character code,
but it can also be a singleton list, an atom of length 1 or a string of length 1.
If it is a variable, it is bound to 0'0 if InLexi can be interpreted as a number
and to 0' , (space) otherwise.

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

?- lexi_n( 123, 8, PadC, + Atom ).
PadC = 48,
Atom = '00000123'.

?- lexi_n( `123`, 8, PadC, + Atom ).
PadC = 48,
Atom = '00000123'.

?- lexi_n( "123", 8, PadC, + Atom ).
PadC = 48,
Atom = '00000123'.

?- lexi_n( `123`, 8, '9', + Atom ).
Atom = '99999123'.

?- lexi_n( `2`, 3, 0'0,  & Atom ).
Atom = "002".

==

@author nicos angelopoulos
@version  0.1 2014/03/17
@version  0.2 2022/11/06, this used to be codes_n_digits/3.
@version  0.3 2023/01/02, allow PadC to be versatile + doc fixes
@see lexi/2
@see n_digits_integer_codes/3

*/
lexi_n( ILexi, N, PadCin, OLexi ) :-
     lexi( ILexi, -(ICodes) ),
	length( ICodes, ILen ),
	Pad is max(N-ILen,0),
     lexi_n_pad_code( PadCin, ICodes, PadC ),
     ( var(PadCin) -> PadC = PadCin; true ),
	findall( PadC, between(1,Pad,_), PadL ),
	Del is max(ILen-N,0),
	break_nth( Del, ICodes, _DCodes, KCodes ),
	append( PadL, KCodes, Codes ),
     lexi( Codes, OLexi ).

lexi_n_pad_code( PadCin, _CodesIn, PadC ) :-
     is_list(PadCin), 
     !,
     PadCin = [PadC].
lexi_n_pad_code( PadCin, _CodesIn, PadC ) :-
     ground( PadCin ),
     !,
     ( number(PadCin) -> 
          PadC = PadCin
          ;
          ( atom(PadCin) ->
               atom_codes(PadCin,[PadC])
               ;
               string_codes(PadCin,[PadC])
          )
     ).
lexi_n_pad_code( _PadCin, CodesIn, PadC ) :-
     ( catch(number_codes(_,CodesIn),_,fail) -> 
          PadC = 0'0
          ;
          PadC = 0' 
     ).
