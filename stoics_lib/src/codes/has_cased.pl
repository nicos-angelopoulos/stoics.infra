:- lib(lexi/2).
:- lib(known/1).
:- lib(message_report/3).

/** has_cased( +Object, +Case, -Cased )

Polymorphic Object has cased subparts of case, Case, that is returned in Cased.

Predicate is polymorphic in Object: string, atom, number, term or codes. 
By default Cased is returned as a list of codes, however, other forms can be asked for using 
the shape grammar of lexi/2.

Case can be one of _up_ or _upper_ and _down_, _low_ or _lower_ and _digit_.
Alternatively, Case can be a list of types recognised by code_type/2, in which case Cased
contains all codes/text that satisfy at least one of the given Case types.

==
?- has_cased( 'Bone Marrow', up, UpCased ).
UpCased = [66, 77].

?- has_cased( 'Bone Marrow', up, +UpCased ).
UpCased = 'BM'.

?- has_cased( "Bone Marrow", down, +DwCased ).
DwCased = onearrow.

?- has_cased( 123, down, +DwCased ).
DwCased = ''.

?- has_cased( 123, digit, DwCased ).
DwCased = [49, 50, 51].

?- has_cased( 123, digit, #(DwCased) ).
DwCased = 123.

?- has_cased( "Bone Marrow", [lower,space], +DwCased ).
DwCased = 'one arrow'.

?- has_cased( "Bone Marrow", towards, +DwCased ).
ERROR: stoics_lib:stoics_lib:has_cased_codes/3: Token: towards, is not a recognisable: value in [upper,up,down,low,lower,digit]

==

@author nicos angelopoulos
@version  0.1 2022/10/15
@see lexi/2

*/
has_cased( Obj, Case, Has ) :-
     ground( Obj ),
     !,
     lexi( Obj, ObjCs ),
     ( is_list(Case) -> 
          has_cased_codes_std(ObjCs,Case,HasCs)
          ;
          once( known( stoics_lib:has_cased_codes(Case,ObjCs,HasCs) ) )
     ),
     lexi( Has, HasCs ).
has_cased( Obj, _Case, _Has ) :-
     Format = 'has_cased/3: First argument should be ground. Found: ~w',
     message_report( Format, [Obj], error ),
     abort.

has_cased_codes( upper, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, [upper], HasCs ).
has_cased_codes( up, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, [upper], HasCs ).
has_cased_codes( down, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, [lower], HasCs ).
has_cased_codes( low, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, [lower], HasCs ).
has_cased_codes( lower, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, [lower], HasCs ).
has_cased_codes( digit, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, [digit], HasCs ).

has_cased_codes_std( [], _CodeTypes, [] ).
has_cased_codes_std( [C|Cs], CodeTypes, HasCs ) :-
     ( (member(CodeType,CodeTypes),code_type(C,CodeType)) ->
          HasCs = [C|TasCs]
          ;
          HasCs = TasCs
     ),
     has_cased_codes_std( Cs, CodeTypes, TasCs ).
