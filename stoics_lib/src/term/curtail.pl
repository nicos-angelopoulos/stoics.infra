
:- lib(term_type/2).
:- lib(term_length/2).

/** curtail( +Term, +Max, -Curtailed ).

Chop Term to a possible maximum of Max (>0) length.<br>
If Term is shorter, Curtail is unified to Term and the call succeeds.

==
?- curtail( [a,b,c], 2, L ).
L = [a, b].
?- curtail( x(a,b,c), 2, C ).
C = x(a, b).
?- curtail( X, 2, V ).
X = V.
?- curtail( abc, 0, V ).
false.
?- curtail( abc, 2, V ).
V = ab.
==

@author nicos angelopous
@version  0.1 2017/11/21

*/
curtail( Term, Max, Curt ) :-
    integer( Max ),
    Max > 0,
    term_length( Term, Len ),
    curtail_max( Len, Max, Term, Curt ).

curtail_max( Len, Max, Term, Curt ) :-
    Max < Len,
    !,
    term_type( Term, TType ),
    curtail_term_max( TType, Term, Max, Curt ).
curtail_max( _Len, _Max, Term, Term ).

curtail_term_max( list, List, Max, Curt ) :-
    curtail_list_prefix( Max, List, Curt ).
curtail_term_max( compound, Term, Max, Curt ) :-
    Term =.. [Name|Args],
    curtail_list_prefix( Max, Args, Crgs ),
    Curt =.. [Name|Crgs].
curtail_term_max( string, String, Max, Curt ) :-
    string_codes( String, Codes ),
    curtail_list_prefix( Max, Codes, CurtCs ),
    string_codes( Curt, CurtCs ).
curtail_term_max( atom, Atom, Max, Curt ) :-
    atom_codes( Atom, Codes ),
    curtail_list_prefix( Max, Codes, CurtCs ),
    atom_codes( Curt, CurtCs ).
curtail_term_max( number(_), Numb, Max, Curt ) :-
    number_codes( Numb, Codes ),
    curtail_list_prefix( Max, Codes, CurtCs ),
    number_codes( Curt, CurtCs ).
curtail_term_max( var, Var, _Max, Var ).
curtail_term_max( dict, Dict, _Max, Dict ).

curtail_term_max( Other, Max, _Curt ) :-
    throw( inconsistent_term_and_length_in_curtail(Other,Max) ).

curtail_list_prefix( 0, _List, [] ) :- !.
curtail_list_prefix( I, [H|Tail], [H|Rest] ) :-
    G is I - 1,
    curtail_list_prefix( G, Tail, Rest ).
