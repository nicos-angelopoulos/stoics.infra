:- lib( os_term/2 ).

/**  os_slashify( -Path, +Slashed ).
     os_slashify( +Path, -Slashed ).

Convert between Slashify and ensure de-slashified versions of os entry names.
The predicate does not check the arguments map to os entries it works purely
on the atomic or string representation. 
Path may be represented by an os slash-term structure or alias compound
(see os_term/2) but Slashed is always either atomic (in most cases) or 
string in the case where Path is string. You can force casting as per os_cast/2.

     0.2@2011/10/28, '' now goes to '', not to '/'

	0.3@2013/10/06, now also goes -Path +Slashed

	0.4@2014/10/06, changed predicate name from slashify/2

==
?- os_slashify( a, A ).
A = 'a/'.

?- os_slashify( "a", A ).
A = "a/".

?- os_slashify( A, 'a/' ).
A = a.

?- os_slashify( A, 'a' ).
A = a.

?- os_slashify( library(csv), Sla ).
Sla = '/home/nicos/pl/lib/src/csv/'.

?- os_slashify( /tmp/abc, &(Sla) ).
Sla = "/tmp/abc/".

?- os_slashify( /tmp/abc, Atom ).
Atom = '/tmp/abc/'.

?- os_slashify( /tmp/abc, \(Term) ).
Term = /tmp/abc.

==

@author nicos angelopoulos
@version  0.5 2016/2/23

*/

os_slashify( OsPath, Slashed ) :-
	ground( OsPath ),
	!,
	os_name( OsPath, PathType ),
	os_slashify_path( PathType, OsPath, Slashed ).

os_slashify( Path, Slashed ) :-
	ground( Slashed ),
	os_name( Slashed, Type ),
	os_de_slashify( Type, Slashed, Path ).

os_de_slashify( string, Slashed, Path ) :-
	atom_string( '/', Slash ),
	once( ( string_concat(SPath,Slash,Slashed)
	        ; SPath = Slashed ) ),
	os_cast( SPath, Path ).
os_de_slashify( atom, Slashed, Path ) :-
	once( ( atom_concat(SPath,'/',Slashed)
	        ; SPath = Slashed ) ),
	os_cast( SPath, Path ).
os_de_slashify( slash, Slashed, Path ) :-
	os_term( ASlashed, Slashed ),
	os_de_slashify( atom, ASlashed, APath ),
	os_cast( APath, Path ).
os_de_slashify( alias, Slashed, Path ) :-
	once( absolute_file_name( Slashed, Atomed ) ),
	os_de_slashify( atom, Atomed, APath ),
	os_cast( APath, Path ).

os_slashify_path( string, OsPath, Slashed ) :-
	atom_string( '/', Slash ),
	string_concat( _DeSlashed, Slash, OsPath ), 
	!,
	os_cast( OsPath, Slashed ).
os_slashify_path( string, OsPath, Slashed ) :-
	atom_string( '/', Slash ),
	string_concat( OsPath, Slash, SlashedPrv ),
	os_cast( SlashedPrv, Slashed ).
os_slashify_path( atom, OsPath, Slashed ) :-
	atom_concat( _, '/', OsPath ),
	!,
	os_cast( OsPath, Slashed ).
os_slashify_path( atom, OsPath, Slashed ) :-
	atom_concat( OsPath, '/', SlashedPrv ),
	os_cast( SlashedPrv, Slashed ).
os_slashify_path( slash, OsPath, Slashed ) :-
	os_term( AOsPath, OsPath ),
	os_slashify_path( atom, AOsPath, ASlashed ),
	os_cast( ASlashed, Slashed ).
os_slashify_path( alias, OsPath, Slashed ) :-
	absolute_file_name( OsPath, Abs ),
	os_slashify_path( atom, Abs, SlashedPrv ),
	os_cast( SlashedPrv, Slashed ).
