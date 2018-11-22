/** lib_load( +Cxt, +Pn, +Pa, +Opts ).

  Load predicate into module Cxt. Assumes all index loading and attaching
  has already occurred.

Opts
 * repo(Repo)
   only look for the predicate in named Repo-sitory

*/
lib_load( Cxt, Pn, Pa, Opts ) :-
    lib_load_repo( Repo, Cxt, Opts ),                % Repo will be user if repo() not in Opts
    lib_load_repo( Repo, Cxt, Pn, Pa, Opts ).

lib_load_repo( _Repo, Cxt, Pn, Pa, _Opts ) :-
    current_predicate( Cxt:Pn/Pa ),             % fixme: can check if source file is from Repo...
    !,
    Mess = 'Not loading anything for predicate:~w as it already exists',
    debug( lib, Mess, Cxt:Pn/Pa ).
lib_load_repo( Repo, Cxt, Pn, Pa, _Opts ) :-
    current_predicate( Repo:Pn/Pa ),
    !,
    Mess = 'Not loading anything for predicate: ~w as it already exists (context: ~w)',
    debug( lib, Mess, [Repo:Pn/Pa,Cxt] ),
    lib_load_repo_defined( Repo, Cxt, Pn, Pa ).
lib_load_repo( Repo, Cxt, Pn, Pa, _Opts ) :-
    debug( lib, 'lib_load_repo Repo var: ~w, Cxt: ~w', [Repo,Cxt] ),
    findall( Repo1-AbsF, ( 
                         (  lib_tables:lib_attached_indices(Cxt,Repo1),
                            debug( lib, 'trying attached repo: ~w', Repo1 ),
                            lib_tables:lib_index(Pn,Pa,Repo1,AbsF)
                          ; 
                            ( once(Repo == user;Repo == Cxt) ),
                            lib_tables:lib_loaded_index(Cxt,_File),
                            lib_tables:lib_index(Pn,Pa,Repo,AbsF),
                            Repo1 = Repo
                         )
                         % above ensures Repo indices are loaded too
                   ),
                            RAbsFsAll
           ),
    sort(  RAbsFsAll, RAbsFs ),
    lib_load_files_indexed( RAbsFs, Cxt, Repo, Pn, Pa ).

% if no files where found via indexes, then...
lib_load_files_indexed( [], Cxt, Repo, Pn, Pa ) :- % caution: Repo may be unbound
    % ... try the homonyms
    findall( Handle-AbsF, ( 
                        ( lib_tables:lib_attached_homonyms(Cxt,Repo),
                          lib_tables:lib_homonym(Pn,Repo,AbsF),
                          Handle=Repo
                        )
                        ;
                        ( lib_tables:lib_homonym(Pn,Cxt,AbsF), Handle=Cxt )
                   ),
                        RAbsFsAll ), 
    sort(  RAbsFsAll, RAbsFs ),
    lib_load_files_homonyms( RAbsFs, Cxt, Repo, Pn, Pa ).
lib_load_files_indexed( [Repo-AbsF], Cxt, _ARepo, Pn, Pa ) :-
    !,
    Mess = 'Loading source to: ~w, from: ~w, and index file: ~w',
    debug( lib,  Mess, [Cxt,Repo:Pn/Pa,AbsF] ),
    lib_load_file_context( Cxt, Repo, Pn, Pa, AbsF ).
lib_load_files_indexed( [F1,F2|Fs], Cxt, Repo, Pn, Pa ) :-
    Mess = 'Multiple sources for: ~w, for predicate: ~w, sources:~w',
    lib_message_report( Mess, [Cxt,Repo:Pn/Pa,[F1,F2|Fs]], error ),
    abort.

lib_load_files_homonyms( [], Cxt, Repo, Pn, Pa ) :-
    Mess = 'No sources found for context: ~w, and predicate: ~w',
    lib_message_report( Mess, [Cxt,Repo:Pn/Pa], error ),
    abort.
lib_load_files_homonyms( [Repo-AbsF], Cxt, _ARepo, Pn, Pa ) :-
    !,
    Mess = 'Loading source to: ~w, from: ~w, and homonym file: ~w',
    debug( lib,  Mess, [Cxt,Repo:Pn/Pa,AbsF] ),
    lib_load_file_context( Cxt, Repo, Pn, Pa, AbsF ).
lib_load_files_homonyms( [F1,F2|Fs], Cxt, Repo, Pn, Pa ) :-
    Mess = 'Multiple sources for: ~w, from single locator: ~w, sources:~w',
    lib_message_report( Mess, [Cxt,Repo:Pn/Pa,[F1,F2|Fs]], error ),
    abort.

lib_load_repo( Repo, _Cxt, Opts ) :-
    memberchk( repo(Repo), Opts ),
    !.
lib_load_repo( Repo, _Cxt, _Opts ) :-
    ground( Repo ),
    !.
lib_load_repo( Cxt, Cxt, _Opts ).

lib_load_repo_defined( Repo, Cxt, Pn, Pa ) :-
    functor( Phead, Pn, Pa ),
    predicate_property( Repo:Phead, exported ),
    !,
    % fixme: oh dear. does this work ?
    debug( lib, '~w', Cxt:import( Repo:Pn/Pa ) ),
    Cxt:import( Repo:Pn/Pa ).
lib_load_repo_defined( Repo, Cxt, Pn, Pa ) :-
    export( Repo:Pn/Pa ),
    % fixme: oh dear. does this work ?
    debug( lib, '~w', Cxt:import( Repo:Pn/Pa ) ),
    Cxt:import( Repo:Pn/Pa ).

lib_load_file_context( Cxt, Repo, Pn, Pa, File ) :-
    Setup = asserta( lib_tables:lib_context(Repo,Root) ),
    Goal  = ensure_loaded(Repo:File),
    Clean = ( once(retract(lib_tables:lib_context(Repo,Root))) ),
    setup_call_cleanup(Setup, Goal, Clean),
    % fixme: does this work? for non-user Cxt too ?
    lib_load_import( Cxt, Repo, Pn, Pa ).

lib_load_import( Cxt, Cxt, _Pn, _Pa ) :-
    !.
lib_load_import( Cxt, Repo, Pn, Pa ) :-
    % Cxt:import(  Repo:Pn/Pa ).
    functor( Phead, Pn, Pa ),
    predicate_property( Repo:Phead, exported ),
    !,
    % fixme: oh dear. does this work ?
    debug( lib, 'import from exported: ~w, in context: ~w', [import(Repo:Pn/Pa),Cxt] ),
    Cxt:import( Repo:Pn/Pa ).
lib_load_import( Cxt, Repo, Pn, Pa ) :-
    debug( lib, 'exporting: ~w', export( Repo:Pn/Pa ) ),
    export( Repo:Pn/Pa ),
    % fixme: oh dear. does this work ?
    debug( lib, 'import from explicitly exported: ~w', Cxt:import( Repo:Pn/Pa ) ),
    Cxt:import( Repo:Pn/Pa ).
