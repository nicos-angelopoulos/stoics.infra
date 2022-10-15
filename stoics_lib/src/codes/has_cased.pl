:- lib(message_report/3).
:- lib(known/1).

/** has_cased( +Object, +Case, -Cased )

Polymorphic Object has cased subparts of case, Case, that is returned in Cased.

Predicate is polymorphic in Object: string, atom, number, term or codes with Cased be returned 
in same type. Case can be one of _up_ or _upper_ and _down_, _low_ or _lower_ and _digit_.

==
?- has_cased( 'Bone Marrow', up, UpCased ).

?- has_cased( "Bone Marrow", down, DwCased ).

?- has_cased( 123, down, DwCased ).

?- has_cased( "Bone Marrow", down, DwCased ).

==

@author nicos angelopoulos
@version  0.1 2022/10/15

*/
has_cased( Obj, Case, Has ) :-
     ground( Obj ),
     !,
     lexi( Obj, ObjCs ),
     known( stoics_lib:has_cased_codes(Case,ObjCs,HasCs) ),
     atom_codes( Has, HasCs ).
has_cased( Obj, _Case, _Has ) :-
     Format = 'has_cased/3: First argument should be ground. Found: ~w',
     message_report( Format, [Obj], error ),
     abort.

has_cased_codes( upper, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, upper, HasCs ).
has_cased_codes( up, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, upper, HasCs ).
has_cased_codes( down, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, lower, HasCs ).
has_cased_codes( low, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, lower, HasCs ).
has_cased_codes( lower, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, lower, HasCs ).
has_cased_codes( digit, ObjCs, HasCs ) :-
     has_cased_codes_std( ObjCs, digit, HasCs ).

has_cased_codes_std( [], _CodeType, [] ).
has_cased_codes_std( [C|Cs], CodeType, HasCs ) :-
     ( code_type(C,CodeType) ->
          HasCs = [C|TasCs]
          ;
          HasCs = TasCs
     ),
     has_cased_codes_std( Cs, CodeType, TasCs ).
