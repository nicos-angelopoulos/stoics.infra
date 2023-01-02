:- lib(message_report/3).

/** lexi( ?Lexi, ?CodeOr ).

Convert any Lexi-cographical object to codes or other shaped form.

If CodeOr is a variable then Lexi is casted to codes.

Casts- mostly for Lexi, but work on CodesOr, if you are so inclined.
  * +(Lex)
    casts to atoms
  * &(Lex)
    casts to strings
  * -(Lex)
    casts to codes
  * #(Lex)
    casts to number

This is a subset to os_lib casts, although here we also use code lists
(something that should be propagated to os_lib).

==
?- lexi('Bone Marrow',Codes).
Codes = [66, 111, 110, 101, 32, 77, 97, 114, 114|...].

?- atom_codes('Bone Marrow',Codes),lexi(+Atom,Codes).
Codes = [66, 111, 110, 101, 32, 77, 97, 114, 114|...],
Atom = 'Bone Marrow'.

?- atom_codes('Peripheral Blood',Codes),lexi(&String,Codes).
Codes = [80, 101, 114, 105, 112, 104, 101, 114, 97|...],
String = "Peripheral Blood".

?- lexi( `Peripheral Blood`, &String ).
String = "Peripheral Blood".

?- atom_codes('Peripheral Blood',Codes),lexi(-Lex,Codes).
Codes = Lex, Lex = [80, 101, 114, 105, 112, 104, 101, 114, 97|...].

?- lexi( 123, Codes ).
Codes = [49, 50, 51].

?- lexi( 123, &String ).
String = "123".

?- lexi(a(term),Codes).

?- lexi(a(term),+Atom).
Atom = 'a(term)'.

?- lexi("Bone Marrow",Codes).
Codes = [66, 111, 110, 101, 32, 77, 97, 114, 114|...].

?- lexi("Bone Marrow",&String).
String = "Bone Marrow".

==

@author nicos angelopoulos
@version  0.1 2022/10/15
@see lexi_n/4
@see has_cased/3

*/

lexi( Lexin, Lexon ) :-
     ground( Lexin ),
     !,
     lexi_to_codes( Lexin, Codes ),
     lexi_from_codes( Lexon, Codes ).
lexi( Lexi, Codes ) :-
     ground( Codes ),
     !,
     lexi_from_codes( Lexi, Codes ).
lexi( Lexi, Codes ) :-
     Format = 'lexi/2: One of the two arguments should be ground. Found, 1: ~w, 2: ~w',
     message_report( Format, [Lexi,Codes], error ),
     abort.

lexi_from_codes( Lexon, Codes ) :-
     var( Lexon ),
     !,
     Codes = Lexon.
lexi_from_codes( Lexon, Codes ) :-
     lexi_shape_codes( Lexon, Codes ),
     !.
lexi_from_codes( Lexon, Codes ) :-
     Format = 'lexi/2: Cannot map codes: ~w to given 2nd argument: ~w',
     message_report( Format, [Codes,Lexon], error ),
     abort.

lexi_to_codes( Lexi, Codes ) :-
     lexi_shape_codes( Lexi, Codes ),
     !.
lexi_to_codes( Atom, Codes ) :-
     atom( Atom ),
     !,
     atom_codes( Atom, Codes ).
lexi_to_codes( String, Codes ) :-
     string( String ),
     !,
     string_codes( String, Codes ).
lexi_to_codes( Num, Codes ) :-
     number( Num ),
     !,
     number_codes( Num, Codes ).
lexi_to_codes( Codins, Codouts ) :-
     catch( atom_codes(_,Codins), _, fail ),
     !,
     Codins = Codouts.
lexi_to_codes( Term, Codes ) :-
     compound( Term ),
     !,
     term_to_atom( Term, Atom ),
     atom_codes( Atom, Codes ).
lexi_to_codes( Other, _Codes ) :-
     Format = 'lexi/2: Cannot figure out type of 1st argument: ~w',
     message_report( Format, [Other], error ),
     abort.

lexi_shape_codes( +(Atom), Codes ) :-
     !,
     atom_codes( Atom, Codes ).
lexi_shape_codes( &(String), Codes ) :-
     !,
     string_codes( String, Codes ).
lexi_shape_codes( -(InCodes), Codes ) :-
     !,
     InCodes = Codes.
lexi_shape_codes( #(Num), Codes ) :-
     !,
     number_codes( Num, Codes ).
