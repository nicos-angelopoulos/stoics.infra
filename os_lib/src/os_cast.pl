/** os_cast( +Os, -Termplate ).
    os_cast( +Os, +Def, -Termplate ).

Cast Termplate's variable part to the Os entity as defined by
Template's grounded part. If Template is a variable, Def is 
used as the type for casting Os onto the Template variable (via os_type_entity/3).

==
?- os_cast( abc/edf.txt, +Var ).
Var = 'abc/edf.txt'.
?- os_cast( abc/edf.txt, \Var ).
Var = abc/edf.txt.
?- os_cast( abc/edf.txt, @Var ).
Var = abc/edf.txt.
?- os_cast( abc/edf.txt, &(Var) ).
Var = "abc/edf.txt".
?- os_cast( abc/edf.txt, Var ).
Var = 'abc/edf.txt'.
?- os_cast( abc/edf.txt, atom, Var ).
Var = 'abc/edf.txt'.
==

Termplates 

 * +(V)
 converts to atom

 * \(V)
 converts to /-term

 * @(V) 
 leave Os as is, assumes it is an alias-term (it is harder to convert arbitrary
 terms to aliased ones)

 * &(V)
 converts to string


 @see os_type_entity/3 for how the types are converted 
 @tbd /(V) was -(V)

*/
os_cast( Os, Def, Termplate ) :-
	os_termplate_cast( Termplate, Def, Os ).

os_cast( Os, Termplate ) :-
	\+ var(Termplate),
	!,
	os_termplate_cast_non_var( Termplate, Os ).
os_cast( Os, Os ).

os_termplate_cast( Var, Type, Os  ) :-
	var(Var),  % fixme: 
	!,
	os_type_entity( Type, Os, Var ).
os_termplate_cast( Term, _Type, Os  ) :-
	os_termplate_cast_non_var( Term, Os ),
	!.
os_termplate_cast( Term, _Type, Os  ) :-
	ground( Os ),
	!,
	Term = Os.  % == ?
os_termplate_cast( Term, Type, Os  ) :-
	throw( pack_error(os,os_plate_cast/3,cast(Term,Type,Os)) ).

os_termplate_cast_non_var( @(Var), Os ) :-
	os_type_entity( alias, Os, Var ).
os_termplate_cast_non_var( +(Var), Os ) :-
	os_type_entity( atom, Os, Var ).
os_termplate_cast_non_var( \(Var), Os ) :-
	os_type_entity( slash, Os, Var ).
os_termplate_cast_non_var( &(Var), Os ) :-
	os_type_entity( string, Os, Var ).
