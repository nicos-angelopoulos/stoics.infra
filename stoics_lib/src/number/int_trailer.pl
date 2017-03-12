/** int_trailer( +Int, -Trailer ).

Get the writen trailer for a positive integer.

==
?- int_trailer( 1, R ).
R = st.

?- int_trailer( 11, R ).
R = th.

?- int_trailer( 21, R ).
R = st.
==

@author nicos angelopoulos
@version  0.2 2016/12/11

*/
int_trailer( 11, th ) :- !.
int_trailer( 12, th ) :- !.
int_trailer( 13, th ) :- !.
int_trailer( Int, R ) :-  atom_concat( _, 1, Int ), !, R = st.
int_trailer( Int, R ) :-  atom_concat( _, 2, Int ), !, R = nd.
int_trailer( Int, R ) :-  atom_concat( _, 2, Int ), !, R = rd.
int_trailer( _AllOther, 'th' ).
