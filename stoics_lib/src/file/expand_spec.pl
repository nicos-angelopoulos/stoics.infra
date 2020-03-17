
:- lib( arity/3 ).

/** expand_spec(+FileSpec, -Expanded).

   Expand the file specification FileSpec to a simple 

   Similar to expand_file_name/2 for Atomic FileSpec but it also 
   works on termed and aliased args (abc/def.pl and abc(def.pl) respectively).

== 
?- expand_spec( '$HOME', Home ).
Home = '/home/na11'

?- expand_spec( src/kv, L  ).
L = 'src/kv'.

?- expand_spec( pack(real), Exp ).
Exp = '/home/na11/lib/swipl/pack/real' ;
Exp = '/usr/local/users/na11/local/git/lib/swipl-7.5.1/pack/real'.

==

@author nicos angelopoulos
@version  0.1  2017/3/8  (split from other sources)

*/
expand_spec( File, Expanded ) :-
	atomic( File ),
	expand_file_name( File, [Expanded] ),
	!.
expand_spec( File, Expanded ) :-
	% (File = _/_ ; File=  /_ ),
    File = _/_ ,
	!,
	% term_atom( File, Atomic ),
    to_os_atom( File, Atomic ),
	expand_file_name( Atomic, [Expanded] ).
expand_spec( Spec, Expanded ) :-
	% allow for alias(abc/def),
	arity( Spec, Alias, 1 ),
	!,
	arg( 1, Spec, TermOr ),
	expand_alias( Alias, Dir ),
	to_os_atom( TermOr, File ),
	directory_file_path( Dir, File, Expanded ).
expand_spec( Spec, Expanded ) :-
	to_os_atom( Spec, Expanded ),
	!.
expand_spec( File, File ).

% we can probably get off  by using term_atom ?
to_os_atom( Term, File ) :-
	\+ var( Term ),
	to_os_atom_1( Term, File ).

to_os_atom_1( A/File, Path ) :-
	!,
	to_os_atom_1( A, Dir ),
	directory_file_path( Dir, File, Path ).
to_os_atom_1( File, File ).

expand_alias( Alias, Dir ) :-
	user:file_search_path( Alias, Dir ),
	atomic( Dir ),
	!.
expand_alias( Alias, Dir ) :-
	user:file_search_path( Alias, Expant ),
	expand_spec( Expant, Dir ).
