
:- lib(op_compare/3).
:- lib(suggests(pack_errors)).

/** has_length( +Term, +Lengthy ).
    has_length( +Term, +Lengthy, +CompOp ).
    has_length( +Term, +Lengthy, +CompOp, +Err ).

Succeeds iff Term has length that is op_compare/3 succesful with Lengthy.
If the predicate does not succeed, it either fails (Err=fail) or throws an error.
Lengthy is either an integer or a term, of which the length is found
via term_length/2. When CompOp is missing is set to =:=.
If Err is anything else than fail it will be transformed to a pack_error/N ball.
If Err is error, then it is ignored and ball is a vanila lengths_mismatch/4, pack_error/1 ball.
Else name and first argument of Err are taken to be the pack and preciate callers and if 3rd and
fourth exist are taken to be token1 and token2 of the length_mismatch/5. 
If pack(pack_errror) is instaled the balls are pretty printed.


==
?- has_length( [a,b,c], 3 ).
true.

?- has_length( [a,b,c], a(d,e,f) ).
true.

?- has_length( [a,b,c], [d,e,f] ).
true.

?- has_length( [a,b,c], 2, =< ).
false.

?- has_length( [a,b,c], 2, > ).
true.

?- has_length( [a,b,c], 2, =<, err(os,os_list/4,art1,art2) ).
ERROR: os:os_list/4: Terms idied by: [a,b,c] and 2, have mismatching lengths: 3 and 2 respectively (=< expected)

==

@author nicos angelopoulos
@version  0.1 2017/8/22

*/
has_length( Term, Lengthy ) :-
    has_length( Term, Lengthy, =:=, fail ).
has_length( Term, Lengthy, Op ) :-
    has_length( Term, Lengthy, Op, fail ).

has_length( Term, Lengthy, Op, Err ) :-
    integer( Lengthy ),
    !,
    has_length_lengthy( Term, TLen ),
    has_length_int( TLen, Lengthy, Term, Lengthy, Op, Err ).
has_length( Term, Lengthy, Op, Err ) :-
    has_length_lengthy( Lengthy, YLen ),
    has_length_lengthy( Term, TLen ),
    has_length_int( TLen, YLen, Term, Lengthy, Op, Err ).

has_length_int( TLen, YLen, _Term, _Lengthy, Op, _Err ) :-
    % has_lengths( TLen, YLen, Op, Err ),
    op_compare( Op, TLen, YLen ),
    !.
has_length_int( TLen, YLen, Term, Lengthy, Op, Err ) :-
    has_length_err( Err, TLen, YLen, Term, Lengthy, Op ).

has_length_lengthy( Lengthy, Length ) :-
    is_list( Lengthy ),
    !,
    length( Lengthy, Length ).
has_length_lengthy( Lengthy, Length ) :-
    compound( Lengthy ),
    !,
    functor( Lengthy, _Name, Length ).
has_length_lengthy( Lengthy, Length ) :-
    atomic( Lengthy ),
    Length is 1.

has_length_err( fail, _TLen, _YLen, _Term, _Lengthy, _Op ) :-
    !,
    fail.
has_length_err( Err, TLen, YLen, Term, Lengthy, Op ) :-
    ( arg(3,Err,Tkn1) -> true; Tkn1 = Term ),
    ( arg(4,Err,Tkn2) -> true; Tkn2 = Lengthy ),
    ( (arg(1,Err,Pack),Pack\=='$null') -> 
        ( (arg(2,Err,Pred),Pred\=='$null') -> 
                throw( pack_error(Pack,Pred,lengths_mismatch(Tkn1,Tkn2,Op,TLen,YLen)) )
                ;
                throw( pack_error(Pack,lengths_mismatch(Tkn1,Tkn2,Op,TLen,YLen)) )
        )
        ;
        throw( pack_error(lengths_mismatch(Tkn1,Tkn2,Op,TLen,YLen)) )
    ).
