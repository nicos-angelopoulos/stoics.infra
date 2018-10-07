%% os_name( +Os, -NameType ).
% 
% NameType is the type of name for Os. The possible types are atom, slash, string and alias.
% The alias type only succeeds if Os is a compound of arity one, and its functor matches a current
% known alias. When Os is a variable, Type is atom.
%
% When Os matches +(_), \(_) , &(_) or @(_) then the corresponding type is _atom_, _slash_ _string_ and alias,
% respectively.
% In this case, then first argument is not inspected.
%
% Types
%  * atom
%  * slash
%  * alias
%  * string
%
%==
% ?- os_name(_,Type).
% Type = atom.
% ?- os_name(abc,Type).
% Type = atom.
% ?- os_name('abc/def',Type).
% Type = atom.
% ?- os_name("abc/def",Type).
% Type = string.
% ?- os_name(abc/def,Type).
% Type = slash.
% ?- os_name(+(_),Type).
% Type = atom.
% ?- os_name(\(_),Type).
% Type = slash.
% ?- os_name(&(_),Type).
% Type = string.
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/9/18, this used to return compound
% @version  0.2 2015/12/10, now we dissect compound to slash and alias. added errors.
%
os_name( Entity, Type ) :-
	var( Entity ),
	!,
	Type = atom.
os_name( /(_Entity), Type ) :-
	!, 
	Type = slash.
os_name( Entity, Type ) :-
	Entity = _/_,
	!,
	Type = slash.
os_name( Entity, Type ) :-
	os_deco_name( Entity, Type ),
	!.
os_name( Os, Type ) :-
	string( Os ),
	!,
	Type = string.
os_name( Entity, Type ) :-
	atomic( Entity ),
	!,
	Type = atom.
os_name( Entity, Type ) :-
	functor( Entity, Alias, 1 ),
	!,
	holds( file_alias_path(Alias,_), HasPath ),
	os_name_alias( HasPath, Entity, Type ).
os_name( Entity, _Name ) :-
	throw( pack_error(os,of_unknown_name(Entity)) ).

os_deco_name( +(_), atom ).
os_deco_name( \(_), slash ).
os_deco_name( @(_), alias ).
os_deco_name( &(_), string ).

os_name_alias( true, _Entity, alias ).
os_name_alias( false, Entity, _ ) :-
    throw( missing_alias(Entitty), os:os_name/2 ).
