
os_exists_defaults( [dir('.'),err(test),not(false),type(any),mode(exist)] ).

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
  * dir(Dir='.')
     parent directory

  * err(Err=test)
    test for report and fail, fail for failing, error for throwing, true for success<br>
    (see options: err(E), on_exit(O) and message(M) in throw/2)

  * not(Not=false)
    reverse polarity, if true require Os not to exist

  * type(Type)
    in addition to Os existing, require file type-ness (dir,link,file,flink,dlink,any).<br>
    Can be used to return the type, when input is a variable. <br>
    Type = base(BaseType) streamline type to either file or dir (see os_type_base/2).

  * mode(Mode=exists)
    one of exist, read, write and append

  * wins_file_exec(WinsFileExec=sys)
    alternatively, use fail for failure and error for error

==
?- os_exists( pack(os_lib/src) ).
true.

?- os_exists( pack(os_lib/src), type(link) ).
false.

?- set_prolog_flag( allow_dot_in_atom, true ).
?- os_exists( pack(os_lib/prolog/os.pl), type(file) ).
true.

?- cd( pack('os_lib/examples/testo') ).

?- os_exists(file1).
true.

?- os_exists( "file1" ).
true

?- os_exists( file2 ).
false.

?- os_exists( file2, err(error) ).
ERROR: os:os_exists/2: OS entity: file2, does not exist

?- os_exists( file2, err(exists) ).
Warning: os:os_exists/2: OS entity: file2, does not exist
false.

?- os_exists( file2, [on_exit(fail),message(warning)] ).
Warning: os:os_exists/2: OS entity: file2, does not exist
false.

?- os_exists( file2, [on_exit(error),message(informational)] ), writeln(later).
% os:os_exists/2: OS entity: file2, does not exist

?- os_exists( file2, not(true) ).
true.

?- os_exists( file1, [not(true),err(error)] ).
ERROR: os:os_exists/2: OS entity: file1, already exists

?- os_exists( file1, type(dir) ).
false.

?- os_exists( file1, [type(dir),err(error)] ).
ERROR: os:os_exists/2: OS entity: file1, not of requested type: dir, but has type: file

?- os_exists( file1, type(flink) ).
true.

?- os_exists( file1, type(link) ).
false.

?- os_exists( dir1/link2, type(link) ).
true.

?- os_exists( dir1/link2, type(base(Base)) ).
Base = file.
==
*/
os_exists( Os ) :-
     os_exists( Os, [] ).

os_exists( OsPrv, Args ) :-
     os_cast( atom, OsPrv, OsAtm ),
     options_append( os_exists, Args, Opts ),
     options( not(Not), Opts ),
     options( dir(Dir), Opts ),
     ( Dir == '.' -> OsAtm = Os ; os_path( Dir, OsAtm, +(Os) ) ),
     os_exists_1( Not, Os, [os(OsPrv)|Opts] ).

os_exists_1( true, Os, Opts ) :-
     os_exists_not( Os, Opts ).
os_exists_1( false, Os, Opts ) :-
     options( [type(Type), mode(Mode)], Opts ),
     ( (\+ var(Type),Type=base(BaseType)) ->
         true
         ;
         OsType = Type
     ),
     os_exists_true( Os, OsType, Mode, Opts ),
     os_type_base( OsType, BaseType ).

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
os_exists_true( Os, _Type, _Mode, Opts ) :-
     throw( os_exists_not(Os), [os:os_exists/2|Opts] ).

os_exists_dir( dir, Os, Mode, Opts ) :-
     \+ read_link( Os, _, _ ),
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
os_exists_dir( any, Os, Mode, Opts ) :-
     os_exists_dir_mode( Mode, Os, Opts ),
     !.
% 18.09.29: fixme: clarify the logic here... ?
os_exists_dir( Other, Os, _Mode, Opts ) :-
     % options( error(ErrB), Opts ),
     os_is_dlink( Os, Which ),
     Error = os_type(Os,Other,Which),
     throw( Error, [os:os_exists/2|Opts] ).

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
     Caupts = [os:os_exists/2,on_true(working_directory(_,Old))|Opts],
     caught( working_directory(Old,Os), os_mode(Os,execute), Caupts ).
os_exists_dir_mode( append, Os, Opts ) :-
     Error = os_mode_undefined(Os,dir,append),
     From = os:os_exists/2,
     throw( Error, [From|Opts] ).

os_exists_dir_mode_read( true, _Os, _Opts ).
os_exists_dir_mode_read( false, Os, Opts ) :-
     Error = os_mode(Os,dir,read),
     throw( Error, [os:os_exists/2|Opts] ).

os_exists_dir_mode_write( true, _Os, _Opts, Out, OsTest ) :-
     close( Out ),
     os_remove( OsTest ).
os_exists_dir_mode_write( false, Os, Opts, _Out, _OsTest ) :-
     % fixme: test OsTest ?
     throw( os_mode(Os,write), [os:os_exists/2|Opts] ).

os_exists_file( file, Os, Mode, Opts ) :-
     \+ read_link( Os, _, _ ),
     !,
     os_exists_file_mode( Mode, Os, Opts ).
os_exists_file( flink, Os, Mode, Opts ) :-
     os_is_flink( Os, _Which ),
     os_exists_file_mode( Mode, Os, Opts ),
     !.
os_exists_file( link, Os, Mode, Opts ) :-
     % read_link( Os, _, _ ),
     os_is_flink( Os, link ),
     os_exists_file_mode( Mode, Os, Opts ),
     !.
os_exists_file( any, Os, Mode, Opts ) :-
     !,
     os_exists_file_mode( Mode, Os, Opts ).
os_exists_file( Unmatched, Os, _Mode, Opts ) :-
     os_is_flink( Os, Which ),
     throw( os_type(Os,Unmatched,Which), [os:os_exists/2|Opts] ).

os_exists_file_mode( exist, _Os, _Opts ) :- !.
os_exists_file_mode( execute, Os, Opts ) :-
     current_prolog_flag( windows, true ),
     options( wins_file_exec(WinsFileExec), Opts ),
     !,
     os_exists_file_mode_wins( WinsFileExec, Os ).
os_exists_file_mode( execute, Os, Opts ) :-
     \+ current_prolog_flag( windows, true ),
     !,
     Error = os_mode(Os,execute),
     caught( access_file(Os,execute), Error, [os:os_exists/2|Opts] ).
os_exists_file_mode( Mode, Os, Opts ) :-
     Caupts = [os:os_exists/2,on_true(close(Out))|Opts],
     caught( open(Os,Mode,Out), os_mode(Os,Mode), Caupts ).

os_exists_file_mode_wins( fail, _Os ) :-  !, fail.
os_exists_file_mode_wins( error, Os ) :-  !, 
     Type = 'file (in windows)',
     throw( os_mode_undefined(Os,Type,execute), os:os_exists/2 ).
os_exists_file_mode_wins( _, Os ) :-  % sys is the default
     current_prolog_flag( windows, true ),
     !,
     access_file( Os, execute ).

os_exists_not( Os, Opts ) :-
     options( dir(Dir), Opts ),
     ( Dir == '.' -> Os = AbsOs; os_path( Dir, Os, +(AbsOs) ) ),
     \+ exists_file( AbsOs ),
     \+ exists_directory( AbsOs ),
     !.
os_exists_not( _Os, Opts ) :-
     memberchk( os(Os), Opts ),
     throw( os_exists(Os), [os:os_exists/2|Opts] ).

os_is_dlink( Os, dir ) :-
     exists_directory( Os ),
     \+ read_link( Os, _, _Target ),
     !.
os_is_dlink( Os, link ) :-
     read_link( Os, _Target, Abs ),
     exists_directory( Abs ).

os_is_flink( Os, file ) :-
     exists_file( Os ),
     \+ read_link( Os, _, _Target ),
     !.
os_is_flink( Os, link ) :-
     exists_file( Os ),
     read_link( Os, Target, _ ),
     os_path( Dir, _, Os ),
     os_path( Dir, Target, Destination ),
     exists_file( Destination ).
