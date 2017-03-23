
os_exists_defaults( [error(fail),not(false),type(any),mode(exist)] ).

/** os_exists( +Os ).
    os_exists( +Os, +Opts ).

True if Os exists as an object in the filestore. When mode is requested,
the predicate goes to the source and tries to effect operations of the 
appropriate mode to establish permissions. The only deviation is 
permission execute on files. By default the predicate uses 
access_file(Os,execute). See option WinsFileExec.

The predicate tries to stay compatible with system predicates, but it does
introduces two new file types: flink and dlink, for file point link or file, and
directory pointing link or directory.

Opts
  * error(Err=fail)
    fail for failing, error for throwing an error and ignore for ignoring

  * not(Not=false)
    reverse polarity, if true require Os not to exist

  * type(Type)
    in addition to existance require file type-ness (dir,link,file,flink,dlink,any)

  * mode(Mode=exists)
    one of exist, read, write and append

  * wins_file_exec(WinsFileExec=sys)
    alternatively, use fail for failure and error for error

==
?- cd( pack(os_lib/src) ).
?- shell( 'ln -s os_file.pl os_file_ex_tmp.pl' ).

?- os_exists( 'os_file.pl' ).
true.

?- os_exists( 'wrong_file.pl' ).
false.

?- os_exists( 'wrong_file.pl', error(true) ).
ERROR: os:os_exists/2: OS entity: wrong_file.pl, does not exist

?- os_exists( os_file.pl, type(file) ).
true.

?- os_exists( os_file.pl, type(link) ).
false.

?- os_exists( os_file.pl, [type(link),error(true)] ).
ERROR: os:os_exists/2: OS entity: os_file.pl, not of requested type: link, but has type: file

?- os_exists( os_file.pl, type(flink) ).
true.

?- os_exists( os_file_ex_tmp.pl, type(flink) ).
true.

?- os_exists( os_file_ex_tmp.pl, type(link) ).
true.

?- os_exists( os_file.pl, not(true) ).
false.

?- os_exists( os_file.pl, [not(true),error(true)] ).
ERROR: os:os_exists/2: OS entity: os_file.pl, already exists

?- os_remove( 'os_file_ex_tmp.pl' ).

?- os_exists( pack(os_lib/src) ).
true.

?- os_exists( pack(os_lib/src), type(link) ).
ERROR: os:os_exists/2: OS entity: /usr/local/users/nicos/local/git/lib/swipl-7.3.16/pack/os/src, not of requested type: link, but has type: dir

?- set_prolog_flag( allow_dot_in_atom, true ).
?- os_exists( pack(os_lib/prolog/os.pl), type(file) ).
true.

?- os_exists( os_exists.pl ).
true
?- os_exists( "os_exists.pl" ).
true

?- shell( 'touch /tmp/a_file.txt' ).
?- os_exists( /tmp/a_file.txt ).
true.
==
*/
os_exists( Os ) :-
	os_exists( Os, [] ).

os_exists( OsPrv, Args ) :-
	os_cast( OsPrv, atom, Os ),
	options_append( os_exists, Args, Opts ),
	options( not(Not), Opts ),
	os_exists_1( Not, Os, [os(OsPrv)|Opts] ).

os_exists_1( true, Os, Opts ) :-
	os_exists_not( Os, Opts ).
os_exists_1( false, Os, Opts ) :-
	options( [type(Type), mode(Mode)], Opts ),
	os_exists_true( Os, Type, Mode, Opts ).

os_exists_true( Os, Type, Mode, Opts ) :-
	exists_file( Os ),
	!,
	os_exists_file( Type, Os, Mode, Opts ).
os_exists_true( Os, Type, Mode, Opts ) :-
	exists_directory( Os ),
	!,
	os_exists_dir( Type, Os, Mode, Opts ).
% SWI's exists_file/1 fails on dangling links
os_exists_true( Os, Type, Mode, Opts ) :-
	read_link( Os, _, _ ),
	!,
	os_exists_file( Type, Os, Mode, Opts ).
os_exists_true( _Os, _Type, _Mode, Opts ) :-
    options( error(ErrB), Opts ),
    os_esists_fail_error( ErrB, Opts ).

os_esists_fail_error( true, Opts ) :-
	memberchk( os(Os), Opts ),
	Error = pack_error(os,os_exists/2,os_exists_not(Os)),
    options( error(ErrB), Opts ),
	caught( false, Error, report(ErrB) ).
os_esists_fail_error( false, _Opts ) :-
    fail.

os_exists_dir( any, Os, Mode, Opts ) :-
	os_exists_dir_mode( Mode, Os, Opts ),
	!.
os_exists_dir( dir, Os, Mode, Opts ) :-
	os_exists_dir_mode( Mode, Os, Opts ),
	!.
os_exists_dir( dlink, Os, Mode, Opts ) :-
	os_is_dlink( Os, _Which ),
	os_exists_dir_mode( Mode, Os, Opts ),
	!.
os_exists_dir( link, Os, Mode, Opts ) :-
	read_link( Os, _, _ ),
	os_exists_dir_mode( Mode, Os, Opts ),
	!.
os_exists_dir( Other, Os, _Mode, Opts ) :-
    options( error(ErrB), Opts ),
	os_is_dlink( Os, Which ),
	Error = pack_error(os,os_exists/2,os_type(Os,Other,Which)),
    caught( false, Error, report(ErrB) ).

os_exists_dir_mode( exist, _Os, _Opts ) :- !.
os_exists_dir_mode( read, Os, Opts) :-
	catch( directory_files(Os,_), _, Failed=true ),
	holds( var(Failed), Success ),
	os_exists_dir_mode_read( Success, Os, Opts ).
os_exists_dir_mode( write, Os, Opts ) :-
	os_path( Os, '.os_exist_test', OsTest ),
	catch( open(OsTest,write,Out), _, Failed=true ),
	holds( var(Failed), Success ),
	os_exists_dir_mode_write( Success, Os, Opts, Out, OsTest ).
os_exists_dir_mode( execute, Os, Opts ) :-
	catch( working_directory(Old,Os), _, Failed=true ),
	holds( var(Failed), Success ),
	os_exists_dir_mode_execute( Success, Os, Opts, Old ).
os_exists_dir_mode( append, Os, Opts ) :-
	Error = pack_error(os,os_exists/2,os_mode_undefined(Os,dir,append)),
    options( error(ErrB), Opts ),
	caught( false, Error, report(ErrB) ).

os_exists_dir_mode_read( true, _Os, _Opts ).
os_exists_dir_mode_read( false, Os, Opts ) :-
	Error = pack_error(os,os_exists/2,os_mode(Os,dir,read)),
    options( error(ErrB), Opts ),
	caught( false, Error, report(ErrB) ).

os_exists_dir_mode_write( true, _Os, _Opts, Out, OsTest ) :-
	close( Out ),
	os_remove( OsTest ).
os_exists_dir_mode_write( false, Os, Opts, _Out, _OsTest ) :-
	% fixme: test OsTest ?
	Error = pack_error(os,os_exists/2,os_mode(Os,write)),
    options( error(ErrB), Opts ),
	caught( false, Error, report(ErrB) ).

os_exists_dir_mode_execute( true, _Os, _Opts, Old ) :-
	working_directory( _, Old ).
os_exists_dir_mode_execute( false, Os, Opts, _Old ) :-
	Error = pack_error(os,os_exists/2,os_mode(Os,execute)),
	caught( false, Error, Opts ).

os_exists_file( any, Os, Mode, Opts ) :-
	!,
	os_exists_file_mode( Mode, Os, Opts ).
os_exists_file( file, Os, Mode, Opts ) :-
	\+ read_link( Os, _, _ ),
	!,
	os_exists_file_mode( Mode, Os, Opts ).
os_exists_file( flink, Os, Mode, Opts ) :-
	os_is_flink( Os, _Which ),
	os_exists_file_mode( Mode, Os, Opts ),
	!.
os_exists_file( link, Os, Mode, Opts ) :-
	read_link( Os, _, _ ),
	os_exists_file_mode( Mode, Os, Opts ),
	!.
os_exists_file( Unmatched, Os, _Mode, Opts ) :-
	os_is_flink( Os, Which ),
	Error = pack_error(os,os_exists/2,os_type(Os,Unmatched,Which)),
    options( error(ErrB), Opts ),
    caught( false, Error, report(ErrB) ).

os_exists_file_mode( exist, _Os, _Opts ) :- !.
os_exists_file_mode( execute, Os, Opts ) :-
	current_prolog_flag( windows, true ),
	options( wins_file_exec(WinsFileExec), Opts ),
	!,
	os_exists_file_mode_wins( WinsFileExec, Os ).
os_exists_file_mode( execute, Os, Opts ) :-
	\+ current_prolog_flag( windows, true ),
	!,
	Error = pack_error(os_exists,2,os_mode(Os,execute)),
    options( error(ErrB), Opts ),
	caught( access_file(Os,execute), Error, report(ErrB) ).
os_exists_file_mode( Other, Os, Opts ) :-
	catch( open(Os,Other,Out), _, Failed=true ),
	holds( var(Failed), Success ), % Success == true iff writable
	os_exists_file_mode_stream( Success, Out, write, Os, Opts ).
	
os_exists_file_mode_stream( true, Out, _Mode, _Os, _Opts ) :-
	close( Out ).
os_exists_file_mode_stream( false, _Out, Mode, Os, Opts ) :-
	% fixme: check Out is not bound
	Error = pack_error(os,os_exists/2,os_mode(Os,Mode)),
    options( error(ErrB), Opts ),
	caught( false, Error, report(ErrB) ).

os_exists_file_mode_wins( fail, _Os ) :-  !, fail.
os_exists_file_mode_wins( error, Os ) :-  !, 
	Type = 'file (in windows)',
	throw( pack_error(os,os_exists/2,os_mode_undefined(Os,Type,execute)) ).
os_exists_file_mode_wins( _, Os ) :-  % sys is the default
	current_prolog_flag( windows, true ),
	!,
	access_file( Os, execute ).

os_exists_not( Os, _Opts ) :-
	\+ exists_file( Os ),
	\+ exists_directory( Os ),
	!.
os_exists_not( _Os, Opts ) :-
	memberchk( os(Os), Opts ),
	Error = pack_error(os,os_exists/2,os_exists(Os)),
    options( error(ErrB), Opts ),
	caught( false, Error, report(ErrB) ).

os_is_dlink( Os, dir ) :-
	exists_directory( Os ),
	\+ read_link( Os, _, _Target ),
	!.
os_is_dlink( Os, link ) :-
	exists_directory( Os ),
	read_link( Os, Target, _ ),
	exists_directory( Target ).

os_is_flink( Os, file ) :-
	exists_file( Os ),
	\+ read_link( Os, _, _Target ),
	!.
os_is_flink( Os, link ) :-
	exists_file( Os ),
	read_link( Os, Target, _ ),
	exists_file( Target ).
