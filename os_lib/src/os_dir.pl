
%fixme: should merge with os_file_defaults...
os_dir_defaults( Defs ) :-
    % ( (memberchk(dir(InDir),Args),\+ InDir == '.') -> Stem = rel ; Stem = false),
    Defs = [dir('.'), stem(rel), sub(false),dots(false),solutions(single)].

%% os_dir( ?OsDir ).
%% os_dir( ?OsDir, +Opts ).
%
% True iff OsDir is a directory or a link to an existing directory, in the current directory.<br>
% Directories '.' and '..' are not returned. Can be used to enumerate all directories.
% 
% Opts
%   * dir(Dir='.')
%      directory in which to find OsDir.
%   * dots(Dots=false)
%      set to true if dot starting dirs are required<br>
%      note that '.' and '..' are never returned
%   * solutions(Sol=single)
%     or findall for returning a list for solutions
%   * stem(Stem=false)
%      what stem to add to returned files, 
%      rel: relative (default), abs: absolute, false: none
%      note, the default 
%   * sub(Sub=false)
%      find OsDir within sub directories when true
%==
% 
% ?-  cd( pack(os_lib) ).
% true.
% 
% ?- ls.
% % doc/      pack.pl   prolog/   src/      
% true.
% 
% ?- os_dir(Dir), write( Dir ), nl, fail.
% doc
% prolog
% src
% false.
% 
% ?- os_dir(& Dir).
% Dir = "doc" ;
% Dir = "prolog" ;
% Dir = "src" ;
% false.
% 
% ?- os_dir(Os,sub(true)), write(Os), nl, fail.
% doc
% doc/html
% prolog
% src
% src/lib
% false.
% 
% ?- os_dir(Os,[stem(abs),sub(true)]), write(Os), nl, fail.
% /usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/doc/doc
% /usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/doc/html/html
% /usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/prolog/prolog
% /usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src/src
% /usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src/lib/lib
% false.
%
% ?- cd(pack('os_lib/examples/testo')).
% ?- os_dir( Dir ).
% Dir = dir1 ;
% false.
% 
% ?- os_dir( Dir, dots(true) ).
% Dir = '.dodi1' 
% Unknown action: ' (h for help)
% Action? ;
% Dir = dir1 ;
% false.
%
% ?- absolute_file_name( pack(os_lib), OsDir ), working_directory( _, OsDir ).
% OsDir = '.../lib/swipl-7.7.19/pack/os_lib',
% 
% ?- ls.
% % doc/        examples/   pack.pl     prolog/     src/        
% true.
% 
% ?- os_dir( Dir ).
% Dir = doc ;
% Dir = examples ;
% Dir = prolog ;
% Dir = src ;
% false.
% 
% ?- os_dir( Dir, solutions(findall) ).
% Dir = [doc, examples, prolog, src].
%==
%
% @author nicos angelopoulos
% @version  0.2 2016/1/31, this version without ref to lib(os_sub)
% @version  0.3 2018/8/05, added options and harmonized with os_file/2
% @version  0.3 2018/10/1, added option dots(D)
% @version  0.4 2018/11/4, added option solutions(D)
% 
os_dir( OsDir ) :-
    os_dir( OsDir, [] ).

os_dir( OsDir, _ ) :-
	ground( OsDir ),
	!,
	os_exists( OsDir, type(dlink) ).
os_dir( OsDir, Args ) :-
    options_append( os_dir, Args, Opts ),
    options( [solutions(Sol),dir(Dir),sub(Sub),stem(Stem),dots(Dots)], Opts ),
    absolute_file_name( Dir, Abs, [file_type(directory),solutions(first)] ),
    os_dir_sol( Sol, OsDir, '', Dir, Abs, Stem, Dots, Sub ).

os_dir_sol( single, OsDir, Path, Dir, Abs, Stem, Dots, Sub ) :-
    os_dir( OsDir, Path, Dir, Abs, Stem, Dots, Sub ).
os_dir_sol( findall, OsDirs, Path, Dir, Abs, Stem, Dots, Sub ) :-
    findall( OsDir, os_dir(OsDir,Path,Dir,Abs,Stem,Dots,Sub), OsDirs ).

os_dir( OsDir, Rel, Dir, Abs, Stem, Dots, Sub ) :-
    os_cast( Dir, +SysDir ),
	directory_files( SysDir, EntriesUno ),
	sort( EntriesUno, Entries ),
	member( Entry, Entries ),
	% \+ atom_concat( '.', _, Entry ),
    Entry \== '.', Entry \== '..',
    os_path( Abs, Entry, AbsEntry ),
	os_exists( AbsEntry, type(dlink) ),
    os_path( Dir, Entry, Desc ),
    os_path( Rel, Entry, RelOs ),
    os_dir_dot( Dots, Entry ),
    os_dir_obj( Desc, RelOs, Entry, OsDir, Dir, AbsEntry, Stem, Dots, Sub ).

os_dir_dot( true, _Entry ).
os_dir_dot( false, Entry ) :-
    \+ atom_concat( '.', _, Entry ).

os_dir_obj( _Os, Rel, Entry, OsDir, _Dir, Abs, Stem, _Dots, _Sub ) :-    % first return the dir
	% os_exists( Rel, type(flink) ),
    % !,
    ( Stem == false ->
	    os_cast( Entry, OsDir )
        ;
        ( Stem == abs ->
            % os_path( Abs, Entry, Path ),
            os_cast( Abs, OsDir )
            ; % defaulty for all other stem values
            % os_path( Rel, Entry, Path )
            os_cast( Rel, OsDir )
        )
    ).
os_dir_obj( Os, Rel, Entry, OsDir, _Dir, Abs, Stem, Dots, true ) :-  % then recurse into the dir if we have been asked
	% os_exists( Rel, type(dlink) ),
    os_path( Entry, Abs, Rbs ),
    os_dir_sol( single, OsDir, Rel, Os, Rbs, Stem, Dots, true ).

%% os_dirs( -Dirs ).
%% os_dirs( +AtDir, -Dirs ).
%
% Find all directories for which os_dir(Dir) succeeds.<br>
% Opts are passed to os_dirs/2.
%
%==
% ?- cd( pack(os_lib) ).
% ?- os_dirs( Dirs ).
%    Dirs = [doc,prolog, src, doc].
%==
% @author nicos angelopoulos
% @version  0.2 2016/1/31, this version without ref to lib(os_sub)
% @version  0.3 2018/8/05, removed os_dir_dirs/2, now these are simply a findall on os_dir/1,2.
% @see os_dirs/2
%
os_dirs( Dirs ) :-
	findall( Dir, os_dir(Dir), Dirs ).
os_dirs( Dirs, Opts ) :-
	findall( Dir, os_dir(Dir,Opts), Dirs ).
