/** term_length( +Term, -Length ).

Return the length of the term. <br>
For atoms and numbers, the length is the length of the codes list that<br>
comprise the atomic term.<br>
Variables and dicts (for now) are of length 0.

==
?- term_length( [a,b,c], L ).
?- term_length( x(a,b,c), L ).
?- St = "abc", string( St ), term_length( St, L ).
?- term_length( abc, L ).
?- term_length( 123, L ).
L = 3.

?- term_length( X, L ).
L = 0.

==

@author nicos angelopoulos
@version  0.1 2017/11/21
@see  lib(term_type).

*/
term_length( Lengthy, Length ) :-
    is_list( Lengthy ),
    !,
    length( Lengthy, Length ).
term_length( Lengthy, Length ) :-
    compound( Lengthy ),
    !,
    functor( Lengthy, _Name, Length ).
term_length( Lengthy, Length ) :-
    string( Lengthy ),
    !,
    string_length( Lengthy, Length ).
term_length( Lengthy, Length ) :-
    atom( Lengthy ),
    !,
    atom_codes( Lengthy, Codes ),
    length( Codes, Length ).
term_length( Lengthy, Length ) :-
    number( Lengthy ),
    !,
    number_codes( Lengthy, Codes ),
    length( Codes, Length ).
term_length( _Lengthy, 0 ).
    % var( Lengthy ),  or is_dict( Lengthy )
    % !,
    % Length is 0.
