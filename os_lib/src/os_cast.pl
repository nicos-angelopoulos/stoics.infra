/** os_cast( +Os, -Termplate ).
    os_cast( +Def, +Os, -Termplate ).

Cast Termplate's variable part to the Os entity as defined by
Template's grounded part. If Template is a variable, Def is 
used as the type for casting Os onto the Template variable (via os_type_entity/3).
In os_cast/2 version, Def defaults to =|atom|=.

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

?- os_cast( atom, abc/edf.txt, Var ).
Var = 'abc/edf.txt'.
==

Termplates 

 * +(V)
 converts to atom (atom)

 * \(V)
 converts to /-term (slash)

 * @(V) 
 leave Os as is, assumes it is an alias-term<br>
 it is harder to convert arbitrary terms to aliased ones, (alias)

 * &(V)
 converts to string (string)

 @see os_type_entity/3 for how the types are converted 
 @tbd /(V) was -(V)

@version 0.2 2018/10/1  swapped 1 & 2 args in /3 version

*/
os_cast( Def, Os, Termplate ) :-
	os_termplate_cast( Termplate, Def, Os ).

os_cast( Os, Termplate ) :-
	\+ var(Termplate),
	!,
	os_termplate_cast_non_var( Termplate, Os ).
os_cast( Os, Var ) :-
    os_cast( atom, Os, Var ).

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

%% os_type_entity( +Type, +Os, -OsTyped ).
%
% OsTyped is Os entity converted to os_name/2 type Type.
% Type alias leaves Os unchaged.
% 
%==
% ?- os_type_entity( slash, a/b, AB ).
% AB = a/b.
% ?- os_type_entity( slash, 'a/b', AB ).
% AB = a/b.
% ?- os_type_entity( atom, a/b, AB ).
% AB = 'a/b'.
% ?- os_type_entity( alias, a(b), AB ).
% AB = a(b).
% ?- os_type_entity( string, a/b, AB ).
% AB = "a/b".
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/9/18
% @version  0.2 2015/12/10  
% @version  0.3 2018/10/1 
%
os_type_entity( alias, Os, Os ) :- !.
os_type_entity( slash, Os, Typed ) :-
	os_name( Os, FromType ),
	os_type_entity_slash( FromType, Os, Typed ).
os_type_entity( atom, Os, Typed ) :-
	os_name( Os, FromType ),
	os_type_entity_atom( FromType, Os, Typed ).
os_type_entity( string, Os, Typed ) :-
	os_name( Os, FromType ),
	os_type_entity_string( FromType, Os, Typed ).

os_type_entity_slash( atom, Os, Typed ) :-
	os_term( Os, Typed ).
os_type_entity_slash( slash, Typed, Typed ).
os_type_entity_slash( alias, Os, Typed ) :-
	absolute_file_name( Os, Abs ),
	os_term( Abs, Typed ).
os_type_entity_slash( string, Os, Typed ) :-
	os_term( Os, ATyped ),
	atom_string( ATyped, Typed ).

os_type_entity_atom( atom, Typed, Typed ).
os_type_entity_atom( slash, Os, Typed ) :-
	os_term( Typed, Os ).
os_type_entity_atom( alias, Os, Abs ) :-
	absolute_file_name( Os, Abs ).
	% os_term( Atom, Abs ).
os_type_entity_atom( string, String, Os ) :-
	atom_string( Os, String ).

os_type_entity_string( atom, Os, Typed ) :-
	atom_string( Os, Typed ).
os_type_entity_string( slash, Os, Typed ) :-
	os_term( Atom, Os ),
	atom_string( Atom, Typed ).
os_type_entity_string( alias, Os, Typed ) :-
	absolute_file_name( Os, Abs ),
	atom_string( Abs, Typed ).
os_type_entity_string( string, Typed, Typed ).
