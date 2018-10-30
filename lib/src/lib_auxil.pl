/** lib_en_list( +Term, -List ).

Ensures Term is within a list. Useful for allowing 
a single option to be passed without the list brackets.

Consider putting this to public interface here,
and remove from stoics_lib. The rationale is that 
that pack(lib) will be required anyhow to load
lib(stoics_lib:en_list/2).

*/
lib_en_list( List, EnListed ) :-
    is_list(List),
    !,
    EnListed = List.
lib_en_list( List, [List] ).

/** lib_load_file( +FileOrFalse, +Mod, +Opts ).

Load FileOrFalse into Mod, iff FileOrFalse \== false
and Load == true. Where load(Load) is in Opts.

*/
lib_load_file( false, _Mod, _Opts ) :- !.
lib_load_file( File, Mod, Opts ) :-
    memberchk( load(Load), Opts ),
    lib_load_file_opt( Load, Mod, File ).

lib_load_file_opt( true, Mod, File ) :-
    lib_ensure_loaded_file( Mod, File ).
lib_load_file_opt( false, _Mod, _ ).

lib_ensure_loaded_file( Repo, File )  :- 
    Repo \== user,
    lib_tables:lib_repo_index( Repo, XFile ),
    !,
    lib_ensure_loaded_file_exists( File, XFile, Repo ).
lib_ensure_loaded_file( Repo, File ) :-
    debug( lib, 'Loading repository main: ~w', Repo:File ),
    ensure_loaded( Repo:File ),
    asserta( lib_tables:lib_repo_index(Repo,File) ).

lib_ensure_loaded_file_exists( File, File, Repo ) :-
    !,
    Mess = 'Main for repository: ~w is already loaded from:~w',
    debug( lib, Mess, [Repo,File] ).
lib_ensure_loaded_file_exists( File, XFile, Repo ) :-
    Mess = 'In loading main for repo: ~w, loaded:~w vs. new:~w',
    lib_message_report( Mess, [Repo,XFile,File], error ),
    abort.
    
/** lib_reg_repo( +Repo, +Type, +Root, +Load ).
    
 Register Repo as having Type, Root and Load-ing file.
 Aborts with error if Repo is already registered under other Root.

*/
lib_reg_repo( Repo, Type, Root, Load ) :-
    lib_tables:lib_repo( Repo, Type, Root, Load ),
    !.
lib_reg_repo( Repo, _Type, Root, _Load ) :-
    lib_tables:lib_repo( Repo, _XType, XRoot, _XLoad ),
    Repo \== user,  % allow user to have multiple locations... 
    % fixme: make sure the rest of the code handles it correctly though
    Root \== XRoot,
    !,
    Mess = 'Repo: ~w, has registered root: ~w, clashing with: ~w',
    lib_message_report( Mess, [Repo,XRoot,Root], error ),
    abort.
lib_reg_repo( Repo, Type, Root, Load ) :-
    debug( lib, 'Reg: ~w', lib_tables:lib_repo(Repo,Type,Root,Load)),
    assert( lib_tables:lib_repo(Repo,Type,Root,Load) ).

/** lib_loading_context( -Cxt ).

    Establish the loading context either via =lib_context:lib_context(Cxt,Root)=
or =prolog_load_context(module,Mod)+.

*/
lib_loading_context( Cxt ) :-
    lib_tables:lib_context( Cxt, _Root ), 
    !.
lib_loading_context( Cxt ) :-
    prolog_load_context( module, Cxt ),
    !.
lib_loading_context( user ).

/** lib_dir_contents( Dir, Files, Dirs ).

    Files and Dirs are those from Dir excluding . and .. .

*/
lib_dir_contents( Dir, Files, Dirs ) :-
    directory_files( Dir, Oses ),
    exclude( lib_dir_dot, Oses, NonDots ),
    lib_dir_contents_partition( NonDots, Dir, Files, Dirs ).

lib_dir_dot( '.' ).
lib_dir_dot( '..' ).

lib_dir_contents_partition( [], _Dir, [], [] ).
lib_dir_contents_partition( [O|Os], Dir, Files, Dirs ) :-
    directory_file_path( Dir, O, DirO ),
    ( exists_file(DirO) ->
        Files = [DirO|Fs],
        Dirs = Ds
        ; 
        ( exists_directory(DirO) ->
            Dirs = [DirO|Ds],
            Files = Fs
            ;
            Files = Fs,
            Dirs  = Ds
        )
    ),
    lib_dir_contents_partition( Os, Dir, Fs, Ds ).

% :- module( message_report, [message_report/3] ).

%% message_report( Format, Args, Kind ).
%
%  An Swi shortcut for printing messages.
%  The predicate first phrases onto a list the Format message
%  filled by Args, as it would do for debug( _, Format, Args ), 
%  then prints these lines as of Kind (error,warning,debug(_)).
%
%==
% ?- Mess = 'Destination:~w already pointed to:~w, repointing to:~w',
% |    F1 = 'file1', F2 = file2, F3 = file3,
% |    message_report( Mess, [F1,F2,F3], warning ).
%
% Warning: Destination:file1 already pointed to:file2, repointing to:file3
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/02/28
%
lib_message_report( Format, Args, Kind ) :-
	phrase('$messages':translate_message(debug(Format,Args)), Lines),
	print_message_lines(current_output, kind(Kind), Lines).
