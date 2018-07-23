os_file_defaults( [dir('.'),sub(false)] ).

%% os_file( ?File ).
%  os_file( ?File, +Opts ).
%
% True iff File is a file or a link to an existing file, in the current directory.
% Can be used to enumerate all files.
%
% Opts
%   * sub(Sub=false)
%      find files within sub directories when true
%==
% ?- absolute_file_name( pack(os/src), Abs ), os_dir_files( Abs, Files ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.3.16/pack/os/src',
% Files = [os_repoint.pl, os_unique_by_date.pl, os_make_path.pl, os_term.pl, os_path.pl, os_ext.pl, os_abs.pl, os_slashify.pl, os_base.pl|...].
% ?- cd( pack(os_lib) ).
% ?- os_file( File ).
% File = pack.pl ;
% false.
% ?- os_file( & File ).
% File = "pack.pl";
% false.
% 
% ?- os_file( File, sub(true) ), os_ext( png, File ).
% File = 'doc/html/priv-bg.png' ;
% File = 'doc/html/h1-bg.png' ;
% File = 'doc/html/pub-bg.png' ;
% File = 'doc/html/multi-bg.png' ;
% File = 'doc/html/h2-bg.png' ;
% false.
% 
%==
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
% @version  0.2 2018/7/23, added options, and in particular sub(true)
% 
os_file( File ) :-
    os_file( File, [] ).

os_file( File, _Opts ) :-
    % fixme: make it play with Opts
	ground( File ),
	!,
	os_exists( File, type(flink) ).
os_file( File, Args ) :-
    options_append( os_file, Args, Opts ),
    options( sub(Sub), Opts ),
    options( dir(Dir), Opts ),
    os_file( File, Dir, Sub ).

os_file( File, Dir, Sub ) :-
	directory_files( Dir, Entries ),
	member( Entry, Entries ),
    Entry \== '.', Entry \== '..',
    os_path( Dir, Entry, Rel ),
    os_file_obj( Rel, Entry, File, Dir, Sub ).

os_file_obj( Rel, Entry, File, Dir, _Sub ) :-
	os_exists( Rel, type(flink) ),
    !,
    ( Dir == '.' ->
	    os_cast( Entry, File )
        ;
        os_cast( Rel, File )
    ).
os_file_obj( Rel, _Entry, File, _Dir, true ) :-
	os_exists( Rel, type(dlink) ),
    os_file( File, Rel, true ).

%% os_files( -Files ).
%% os_dir_files( +Dir, -Files ).
%
% Collects all files for which os_file(File) succeeds in directory Dir. 
% When Dir is missing it is set to '.'.
%
%==
% ?- absolute_file_name( pack(os/src), Abs ), os_dir_files( Abs, Files ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.3.16/pack/os/src',
% Files = [os_repoint.pl, os_unique_by_date.pl, os_make_path.pl, os_term.pl, os_path.pl, os_ext.pl, os_abs.pl, os_slashify.pl, os_base.pl|...].
%==
%
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
%
os_files( Files ) :-
	findall( File, os_file(File), Files ).

os_dir_files( Dir, Files ) :-
	os_cast( Dir, atom, ADir ),
	working_directory( Old, ADir ),
	os_files( Files ),
	working_directory( _, Old ).
