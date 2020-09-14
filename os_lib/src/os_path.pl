
:- lib( os_slashify/2 ).
:- lib( os_name/2 ).

/** os_path( +Dir, +File, -Path ). 
    os_path( -Dir, -File, +Path ). 
    os_path{ +Parts, -Path ).
    os_path( -Parts, +Path ).

  Mostly a polymorphic directory_file_path/3 nickname.
Also '' is the default rather than '/' when dealing with absolute paths.
The predicate understands all the input types known to os_name/2. 
Path takes its type from Dir and vice versa- when mode is reversed. 
If you require Path to be of specific type regardless of what Dir is
decorate -Path as explained is os_name/2.

==
?- os_path( D, F,  '/abc/def/' ).
D = '/abc',
F = def.

?- os_path( D, F,  "/abc/def/" ).
D = "/abc",
F = "def".

?- os_path( D, F,  /abc/def/ ).
ERROR: Syntax error: Unbalanced operator
ERROR: os_path( D, F,  /abc/def/
ERROR: ** here **
ERROR:  ) . 

?- os_path( D, F,  /abc/def ).
D = /abc,
F = def.

?- os_path( abc/def, ghi.txt, Path ).
Path = abc/def/ghi.txt.

?- os_path( /abc/def, ghi.txt, Path ).
Path = /abc/def/ghi.txt.

?- os_path( '', abc, Abc ).
Abc = abc.

?- directory_file_path( '', abc, Abc ).
   Abc = '/abc'.

?- os_path( Dir, Base, '/abc' ).
Dir = '',
Base = abc.

?- directory_file_path( Dir, Base, '/abc' ).
Dir =  (/),
Base = abc.


?- os_path( hmrn(library(what)), abc, Abc ).
ERROR: pack(os): Nested aliases are not supported yet.
Found: hmrn(library(what)) at position: 1 for predicate: os_path/3

?- file_search_path( hmrn, Hmrn ).
Hmrn = '/home/nicos/ac/14mg/cohorts/hmrn'.

?- os_path( hmrn(what), if, Abc ).
Abc = hmrn('what/if').

?- os_path( hmrn(what/if), not, Abc ).
Abc = hmrn(what/if/not).

?- os_path( hmrn(what/if), not, +Abc ).
Abc = '/home/nicos/ac/14mg/cohorts/hmrn/what/if/not'.

?- os_path( hmrn(what/if), not, \Abc ).
Abc = /home/nicos/ac/'14mg'/hmrn/what/if/not.

?- os_path( hmrn(what/if), not, @Abc ).
Abc = hmrn(what/if/not).

?- os_path( "what/if", foo.txt, Abc ).
Abc = "what/if/foo.txt".

?- os_path( Parts, 'a/b/c.txt' ), os_path( Parts, Rel ).
Parts = [a, b, c.txt],
Rel = 'a/b/c.txt'.

?- os_path( Parts, '/a/b/c.txt' ), os_path( Parts, Rel ).
Parts = ['', a, b, c.txt],
Rel = '/a/b/c.txt'.

==

@author nicos angelopoulos
@version  0.4 2020/9/14
*/
os_path( Parts, Path ) :-
    ground( Path ),
    !,
    os_path_decon( Path, Parts ).
os_path( Parts, Path ) :-
    % do not use at_con/3, see example above...
    atomic_list_concat( Parts, '/', Prov ),
    os_cast( Prov, Path ).
os_path( Dir, File, Path ) :-
    File == '.', 
    !,
    Path = Dir.
os_path( Dir, File, Path ) :-
	ground( Path ),
	% var( Dir ), 
	% var( File ),
	!,
	os_name( Path, Type ),
	os_path_type_de( Type, Path, Dir, File ).

os_path( Dir, File, PathOut ) :-
	ground( Dir ),
	ground( File ),
	os_path_ground( Dir, File, Type, Path ),
	os_cast( Type, Path, PathOut ).

os_path_ground( '', File, Type, Os ) :- 
	!, 
	os_name( File, Type ), 
	os_cast( Type, File, Os ).
os_path_ground( Dire, '', Type, Os ) :- 
	!,
	os_name( Dire, Type ),
	os_cast( Type, Dire, Os ).
os_path_ground( Dir, File, Type, Path ) :-
	os_name( Dir, Type ),
	os_cast( Type, File, Tile ),
	os_path_type( Type, Dir, Tile, Path ).

os_path_type( atom, Dir, File, Path ) :-
	directory_file_path( Dir, File, Path ).
os_path_type( slash, Dir, File, Path ) :-
	Path = Dir/File.
os_path_type( alias, Dir, File, Path ) :-
	Dir =.. [Alias,AArg],
	os_name( AArg, AAtype ),
	os_path_alias_arg( AAtype, Dir, AArg, File, PArg ),
	Path =.. [Alias,PArg].
os_path_type( string, Dir, File, Path ) :-
	atom_string( '/', Slash ),
	string_concat( Slash, File, Sile ),
	string_concat( Dir, Sile, Path ).

os_path_alias_arg( alias, Dir, _AArg, _File, _PArg ) :-  
	!,
	throw( pack_error(os,nested_alias(1,os_path/3,Dir)) ).
os_path_alias_arg( NonAlias, _Dir, AArg, File, PArg ) :-
	os_path_type( NonAlias, AArg, File, PArg ).

/*
os_path_unalias( Dir, Unaliased ) :-
	compound( Dir ),
	Dir =.. [Alias|_],
	user:file_search_path( Alias, _ ),
	absolute_file_name( Dir, Unaliased ),
	!.
os_path_unalias( Dir, Dir ).
*/


os_path_type_de( atom, Path, Dir, File ) :-
	os_slashify( PathNoSlash, Path ),
	directory_file_path( DirPrv, File, PathNoSlash ),
	os_path_type_de_dir( DirPrv, Dir ).
os_path_type_de( slash, DirFile, Dir, File ) :-
	( Dir/File = DirFile -> true; /(File) = DirFile, Dir = (/) ).
os_path_type_de( alias, Aliased, Dir, File ) :-
	% currently nested aliases are not handled by absolute_file_name/2
	% but we might introduce a version that does
	absolute_file_name( Aliased, DirFile ),
	os_path_type_de( atom, DirFile, Dir, File ).
os_path_type_de( string, String, Dir, File ) :-
	atom_string( Atom, String ),
	os_path_type_de( atom, Atom, DirAtom, FileAtom ),
    maplist( os_cast(string), [DirAtom,FileAtom], [Dir,File] ).

os_path_type_de_dir( '/', '' ) :- !.
os_path_type_de_dir( Dir, Dir ).

os_path_decon( Path, Parts ) :-
    os_path( '.', File, Path ),
    Path == File,
    !,
    Parts = [Path].
os_path_decon( Path, Parts ) :-
    os_path( Dir, Base, Path ),
    os_path_decon( Dir, Darts ),
    append( Darts, [Base], Parts ),
    !.
