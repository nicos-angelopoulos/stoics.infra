os_file_defaults( Defs ) :-
    Defs = [dir('.'), stem(rel), sub(false), dots(false),solutions(single)].

%% os_file( ?File ).
%% os_file( ?File, +Opts ).
%
% True iff File is a file or a link to an existing file, in the current directory.<br>
% Can be used to enumerate all files. The order is via sort/2.
%
% Opts
%   * dir(Dir='.')
%      directory in which to find File
%   * dots(Dots=false)
%      set to true if dot starting files are required<br>
%      note that '.' and '..' are never returned
%   * solutions(Sol=single)
%     or findall for returning a list of all solutions
%   * stem(Stem=rel)
%      what stem to add to returned files, 
%      rel: relative (default else), abs: absolute, false: no, stem
%   * sub(Sub=false)
%      find files within sub directories when true
%
%==
% ?- cd(pack('os_lib/examples/testo')).
%
% ?- os_file(File).
% File = file1 ;
%
% ?- os_file( & File ).
% File = "file1".
% 
% ?- os_file(File, sub(true)).
% File = 'dir1/file2' ;
% File = 'dir1/link2' ;
% File = file1.
% 
% ?- os_file(File, dots(true)).
% File = '.dotty1' ;
% File = file1.
% 
%:- absolute_file_name( pack(os_lib), OsDir ), working_directory( Old, OsDir ).
% OsDir = '/usr/local/users/nicos/local/git/lib/swipl-7.7.19/pack/os_lib',
% Old = '/home/nicos/.unison/canonical/sware/nicos/git/github/stoics.infra/'.
% 
% File = pack.pl ;
% false.
% 
% ?- os_file( File, solutions(findall) ).
% File = [pack.pl].
% 
% ?- os_file( File, [solutions(findall),sub(true)] ).
% File = ['doc/Releases.txt', 'doc/html/h1-bg.png', 'doc/html/h2-bg.png', 'doc/html/multi-bg.png', 'doc/html/os.html', 'doc/html/pldoc.css', 'doc/html/priv-bg.png', 'doc/html/pub-bg.png', 'examples/testo/dir1/file2'|...].
% 
% ?- os_file( File, [solutions(single),sub(true)] ).
% File = 'doc/Releases.txt' ;
% File = 'doc/html/h1-bg.png' ;
% File = 'doc/html/h2-bg.png' ;
% File = 'doc/html/multi-bg.png'...
%==
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
% @version  0.2 2018/7/23, added options, dir(Dir) and sub(true)
% @version  0.3 2018/10/1, added option dots(Dots)
% @version  0.4 2018/11/4, added option solutions(Sol)
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
    options( [dir(Dir),dots(Dots),solutions(Sol),stem(Stem),sub(Sub)], Opts ),
    absolute_file_name( Dir, Abs, [file_type(directory),solutions(first)] ),
    os_file_sol( Sol, File, Dir, Abs, Stem, Dots, Sub ).

os_file_sol( single, File, Dir, Abs, Stem, Dots, Sub ) :-
    os_file( File, '', Dir, Abs, Stem, Dots, Sub ).
os_file_sol( findall, Files, Dir, Abs, Stem, Dots, Sub ) :-
    findall( File, os_file(File,'',Dir,Abs,Stem,Dots,Sub), Files ).

os_file( File, Rel, Dir, Abs, Stem, Dots, Sub ) :-
    os_cast( Dir, +SysDir ),
	directory_files( SysDir, EntriesUno ),
    sort( EntriesUno, Entries ),
	member( Entry, Entries ),
    Entry \== '.', Entry \== '..',
    os_file_dot( Dots, Entry ),
    os_path( Dir, Entry, Desc ),
    os_path( Rel, Entry, RelDesc ),
    os_file_obj( Desc, RelDesc, Entry, File, Dir, Abs, Stem, Dots, Sub ).

os_file_dot( true, _Os ).
os_file_dot( false, Os ) :- 
    \+ atom_concat( '.', _, Os ).

os_file_obj( Desc, Rel, Entry, File, _Dir, Abs, Stem, _Dots, _Sub ) :-
	os_exists( Desc, [type(flink),err(test)] ),
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
os_file_obj( Desc, Rel, Entry, File, _Dir, Abs, Stem, Dots, true ) :-
	os_exists( Desc, type(dlink) ),
    os_path( Abs, Entry, Rbs ),
    os_file( File, Rel, Desc, Rbs, Stem, Dots, true ).

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
