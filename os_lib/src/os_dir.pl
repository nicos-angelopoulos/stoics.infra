
%fixme: should merge with os_file_defaults...
os_dir_defaults( Defs ) :-
    % ( (memberchk(dir(InDir),Args),\+ InDir == '.') -> Stem = rel ; Stem = false),
    Defs = [dir('.'), stem(rel), sub(false)].

%% os_dir( ?OsDir ).
%% os_dir( ?OsDir, +Opts ).
%
% True iff OsDir is a directory or a link to an existing directory, in the current directory.<br>
% Directories '.' and '..' are not returned. Can be used to enumerate all directories.
% 
% Opts
%   * dir(Dir='.')
%      directory in which to find OsDir.
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
%==
%
% @author nicos angelopoulos
% @version  0.2 2016/1/31, this version without ref to lib(os_sub)
% @version  0.3 2018/8/05, added options and harmonized with os_file/2
% 
os_dir( OsDir ) :-
    os_dir( OsDir, [] ).

os_dir( OsDir, _ ) :-
	ground( OsDir ),
	!,
	os_exists( OsDir, type(dlink) ).
os_dir( OsDir, Args ) :-
    options_append( os_dir, Args, Opts ),
    options( [dir(Dir),sub(Sub),stem(Stem)], Opts ),
    absolute_file_name( Dir, Abs, [file_type(directory),solutions(first)] ),
    os_dir( OsDir, Dir, Abs, Stem, Sub ).

os_dir( OsDir, Dir, Abs, Stem, Sub ) :-
	directory_files( Dir, EntriesUno ),
	sort( EntriesUno, Entries ),
	member( Entry, Entries ),
	% \+ atom_concat( '.', _, Entry ),
    Entry \== '.', Entry \== '..',
    os_path( Abs, Entry, AbsEntry ),
	os_exists( AbsEntry, type(dlink) ),
    os_path( Dir, Entry, Rel ),
    os_dir_obj( Rel, Entry, OsDir, Dir, AbsEntry, Stem, Sub ).

os_dir_obj( Rel, Entry, OsDir, _Dir, Abs, Stem, _Sub ) :-    % first return the dir
	% os_exists( Rel, type(flink) ),
    % !,
    ( Stem == false ->
	    os_cast( Entry, OsDir )
        ;
        ( Stem == abs ->
            os_path( Abs, Entry, Path ),
            os_cast( Path, OsDir )
            ; % defaulty for all other stem values
            % os_path( Rel, Entry, Path )
            os_cast( Rel, OsDir )
        )
    ).
os_dir_obj( Rel, _Entry, OsDir, _Dir, Abs, Stem, true ) :-  % then recurse into the dir if we have been asked
	% os_exists( Rel, type(dlink) ),
    os_path( Rel, Abs, Rbs ),
    os_dir( OsDir, Rel, Rbs, Stem, true ).

%% os_dirs( -Dirs ).
%% os_dirs( +AtDir, -Dirs ).
%
% Find-alls, all directories for which os_dir(Dir) succeeds.<br>
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
