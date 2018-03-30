
% :- lib(options).  % assume
% :- lib(error).    % autoload...?

os_make_path_defaults( [ debug(false),afresh(false),debug_exists(false),descend(false),parent(_),make_path(true)] ).

/** os_make_path( +Path ).
    os_make_path( +Path, +Opts ).
    os_make_path( -Path, +Opts ).

Mostly as make_directory_path/1, but with options. Also it can generate Path 
from its options.

Opts

  * afresh(Afresh=false)
    when Afresh==true, predicate removes path before creating it afresh

  * debug_exists(DbgExists=false)
    The predicate only reports when directory did not exist by default.
    Turn this to true to print a debug message when the directory did exist
    and Dbg is true

  * debug(Dbg=false)
    as per options_append/4 when true it prints a debugging message. 
    The predicate does not listen to debug(os_make_path), 
    instead it uses options_debug/3.

  * dir(Dir)
    operate on Dir if Path is a variable and Odir is not given

  * odir(Odir)
    operate on Odir if Path is a variable (preceeds over Dir).

  * descend(Desc=false)
    whether to move to newly created directory

  * make_path(MkPath=true)
    do not go on with operation if MkPath is =false=

  * parent(Par)
    returns current directory (useful when Move == true)

==
?- os_make_path( '/tmp/what', debug(true)  ).
% Creating path: '/tmp/what'
true.

?- os_make_path( '/tmp/what', debug(false)  ).
true.

?- os_make_path( '/tmp/what', debug(true)  ).
true.

?- os_make_path( '/tmp/what', debug_exists(true)  ).
true.

?- os_make_path( '/tmp/what', [debug_exists(true),debug(true)]  ).
% Path existed: '/tmp/what'
true.

?- os_make_path( '/tmp/what1', debug(false)  ).
true.

?- os_make_path( '/tmp/what2' ).
true.

?- os_make_path( "/tmp/what4" ).
true.

?- os_make_path( /tmp/what5 ).
true.

?- os_make_path( library(tmp) ).

?- os_make_path( Path, odir('/tmp/what3') ).
Path = '/tmp/what3'.

?- ls('/tmp/what3').
true.

?- os_make_path( Path, true ).

ERROR: Domain error: `Path, first argument in os_path_make/2 ground, or options containing dir/1 or odir/2' expected, found `'Opts'=['$restore'(os_make_path,debug,false),true,debug(false),afresh(false),debug_exists(false)]'

?- os_make_path( '/tmp/new1', [make_path(false),debug(true)] ).
% Skipping creation of path: '/tmp/new1'
true.

==

@see options_append/4  (pack(options))
@see options_debug/3   (pack(options))

*/

os_make_path( Path ) :-
	os_make_path( Path, [] ).

os_make_path( Path, Args ) :-
	options_append( os_make_path, Args, Opts ),
	once( os_make_path_ground(Path,Opts) ),
	% os_term( AtomPath, Path ),
	os_abs( Path, AtomPath ),
	os_make_path_report( AtomPath, Opts ),
	options( descend(Desc), Opts ),
	os_make_path_descend( Desc, Here, AtomPath ),
	memberchk( parent(Here), Opts ).

os_make_path_descend( true, Here, AtomPath ) :-
	working_directory( Here, AtomPath ).
os_make_path_descend( false, Here, _ ) :-
	working_directory( Here, Here ).

os_make_path_ground( Path, _Opts ) :-
	ground(Path).
os_make_path_ground( Path, Opts ) :-
	memberchk( odir(Path), Opts ).
os_make_path_ground( Path, Opts ) :-
	memberchk( dir(Path), Opts ).
os_make_path_ground( _Path, Opts ) :-
	Exp = 'Path, first argument in os_path_make/2 ground, or options containing dir/1 or odir/2',
	domain_error( Exp, 'Opts'=Opts ).

os_make_path_report( Path, Opts ) :-
	exists_directory( Path ), 
	!,
	os_make_path_report_exists( Path, Opts ),
	options( afresh(Afresh), Opts ),
	os_make_path_afresh( Afresh, Path, Opts ).
os_make_path_report( Path, Opts ) :-
	os_make_directory_path( Path, Opts ).

os_make_directory_path( Path, Opts ) :-
    options( make_path(Mk), Opts ),
    os_make_directory_path_bool( Mk, Path, Opts ).

os_make_directory_path_bool( true, Path, Opts ) :-
	Mess = 'Creating path: ~p',
	options_debug( Mess, Path, Opts ),
	make_directory_path( Path ).
os_make_directory_path_bool( false, Path, Opts ) :-
	Mess = 'Skipping creation of path: ~p',
	options_debug( Mess, Path, Opts ).

os_make_path_afresh( true, Path, Opts ) :-
	!,
	delete_directory_and_contents( Path ),
	os_make_directory_path( Path, Opts ).
os_make_path_afresh( _Other, _Path, _Opts ).

os_make_path_report_exists( Path, Opts ) :-
	options( debug_exists(true), Opts ),
	!,
	Mess = 'Path existed: ~p',
	options_debug( Mess, Path, Opts ),
	debug( os_make_path, 'os_make: done', Path ).  
		% fixme, this is only so we don't get annoying warning...that debug topic does not exist
os_make_path_report_exists( _Path, _Opts ).

