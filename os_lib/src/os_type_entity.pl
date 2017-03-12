:- lib( os_term/2 ).

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
