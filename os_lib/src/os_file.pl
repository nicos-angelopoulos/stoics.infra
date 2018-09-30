
os_file_defaults( Defs ) :-
    % ( (memberchk(dir(InDir),Args),\+ InDir == '.') -> Stem = rel ; Stem = false),
    Defs = [dir('.'), stem(rel), sub(false)].

%% os_file( ?File ).
%% os_file( ?File, +Opts ).
%
% True iff File is a file or a link to an existing file, in the current directory.
% Can be used to enumerate all files. The order is via sort/2.
%
% Opts
%   * dir(Dir='.')
%      directory in which to find File
%   * stem(Stem=rel)
%      what stem to add to returned files, 
%      rel: relative (default else), abs: absolute, false: no, stem
%   * sub(Sub=false)
%      find files within sub directories when true
%
%==
% ?- absolute_file_name( pack(os_lib/src), Abs ), os_file( File, dir(Abs) ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = 'LibIndex.pl' ;
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = os_abs.pl ;
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = os_base.pl ;
% ...
%
% ?- absolute_file_name( pack(os_lib/src), Abs ), os_file( File, [dir(Abs),sub(true)] ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = 'LibIndex.pl' ;
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = 'lib/inst_error_full.pl' ;
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = 'lib/os_type_create.pl' ;
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = os_abs.pl ;
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = os_base.pl .
% ... 
%
% ?- cd( pack(os_lib) ).
%
% ?- os_file( File ).
% File = pack.pl ;
% false.
%
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
% ?- absolute_file_name( pack(os_lib/src), Abs ), working_directory(_,Abs), os_file( File, [sub(true),stem(abs)] ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src/LibIndex.pl' ;
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% File = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src/lib/inst_error_full.pl' ;
% ...
%==
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
% @version  0.2 2018/7/23, added options, dir(Dir) and sub(true)
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
    options( stem(Stem), Opts ),
    absolute_file_name( Dir, Abs, [file_type(directory),solutions(first)] ),
    os_file( File, '', Dir, Abs, Stem, Sub ).

os_file( File, Rel, Dir, Abs, Stem, Sub ) :-
    os_cast( Dir, +SysDir ),
	directory_files( SysDir, EntriesUno ),
    sort( EntriesUno, Entries ),
	member( Entry, Entries ),
    Entry \== '.', Entry \== '..',
    os_path( Dir, Entry, Desc ),
    os_path( Rel, Entry, RelDesc ),
    os_file_obj( Desc, RelDesc, Entry, File, Dir, Abs, Stem, Sub ).

os_file_obj( Desc, Rel, Entry, File, _Dir, Abs, Stem, _Sub ) :-
	os_exists( Desc, [type(flink),on_exit( ),
    !,
    ( Stem == false ->
	    os_cast( Entry, File )
        ;
        ( Stem == abs ->
            os_path( Abs, Entry, Path ),
            os_cast( Path, File )
            ; % defaulty for all other stem values
            % os_path( Rel, Entry, Path )
            os_cast( Rel, File )
        )
    ).
os_file_obj( Desc, Rel, Entry, File, _Dir, Abs, Stem, true ) :-
	os_exists( Desc, type(dlink) ),
    os_path( Abs, Entry, Rbs ),
    os_file( File, Rel, Desc, Rbs, Stem, true ).

os_files_defaults( [dir('.'),sub(false)] ).

%% os_files( -Files ).
%% os_files( -Files, +Opts ).
%
% Collects all files for which os_file(File) or os_file(File,Opts) succeed.<br>
% Opts are passed to os_file/2.
% 
%==
% ?- absolute_file_name( pack(os_lib/src), Abs ), os_files( Files, dir(Abs) ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% Files = ['/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src/os_make_path.pl', '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src/os_term.pl', '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src/os_dir_stem_ext.pl', ... ]
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
% @version  0.2 2018/8/05, added options, dir(Dir) and sub(true), removed os_dir_files/2
% see os_file/2
%
os_files( Files ) :-
	findall( File, os_file(File), Files ).
os_files( Files, Opts ) :-
	findall( File, os_file(File,Opts), Files ).
