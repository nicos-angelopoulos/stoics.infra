/** lib_attach_indicies( +AttachB, +Root, +From, +Cxt ).

   Attach indices From (module) Root (directory) into context Cxt
   iff AttachB is set to +true=.

*/
lib_attach_indices( false, _Root, _From, _Cxt ).
lib_attach_indices( true, Root, From, Cxt ) :-
    lib_ensure_loaded_index( Root, From, Cxt ).

lib_ensure_loaded_index( Root, Repo, Cxt ) :-
    ( Sub = ''; lib_src_sub_dir( Sub ) ),
    directory_file_path( Root, Sub, Rub ),
    % fixme: allow users to change file name:
    directory_file_path( Rub, 'LibIndex.pl', LibIdxF ),
    exists_file( LibIdxF ),
    lib_ensure_loaded_index_file( Repo, Cxt, LibIdxF ),
    fail.
lib_ensure_loaded_index( _Root, _Repo, _Cxt ).
    
lib_ensure_loaded_index_file( Repo, Cxt, File ) :-
    lib_tables:lib_loaded_index( Repo, XFile ),
    !,
    lib_ensure_loaded_index_file_exists( Repo, File, XFile ),
    lib_ensure_loaded_index_file_attach( Repo, Cxt ).
lib_ensure_loaded_index_file( Repo, Cxt, LibIdxF ) :-
    Mess = 'Loading index for repo: ~w, from file:~w, into: ~w',
    debug( lib, Mess, [Repo,LibIdxF,Cxt] ),
    lib_load_index_file( LibIdxF, Repo ),
    lib_ensure_loaded_index_file_attach( Repo, Cxt ).

lib_ensure_loaded_index_file_attach( Repo, Cxt ) :-
    Repo \== Cxt, 
    \+ lib_tables:lib_attached_indices(Cxt,Repo),
    !,
    asserta( lib_tables:lib_attached_indices(Cxt,Repo) ).
lib_ensure_loaded_index_file_attach( _Repo, _Cxt ).

lib_ensure_loaded_index_file_exists( Repo, File, File ) :-
    !,
    debug( lib, 'While attaching, repo: ~w, already has loaded index file: ~w', [Repo,File] ).
lib_ensure_loaded_index_file_exists( Repo, File, XFile ) :-
    Mess = 'Clash in attaching repo: ~w, old file:~w, vs. new:',
    debug( lib, Mess, [Repo,XFile,File] ).

lib_load_repo_root_index_file( Repo, Root ) :-
    lib_src_sub_dir( Sub ),
    directory_file_path( Root, Sub, AbsSub ),
    directory_file_path( AbsSub, 'LibIndex.pl', LibIndex ),
    exists_file( LibIndex ),
    !,
    lib_load_index_file( LibIndex, Repo ).

/** lib_load_index_file( +File, +Repo ).

    Load/assert index terms from LibIndex syntaxed File coming from Pack
    and rooted at absolute dir Root.

*/
lib_load_index_file( File, Repo ) :-
    lib_tables:lib_loaded_index( Repo, File ),
    !.

lib_load_index_file( File, Repo ) :-
    directory_file_path( Root, _Base, File ),
    open( File, read, In ),
    read( In, Term ),
    lib_load_index_stream( Term, Repo, Root, In ),
    close( In ),
    % lib_loaded_ensure_assert( Repo, File ).
    assert( lib_tables:lib_loaded_index(Repo,File) ).

/*
lib_loaded_ensure_assert( Repo, File ) :-
    lib_tables:lib_loaded_index( Repo, File ),
    !.
lib_loaded_ensure_assert( Repo, File ) :-
    */

lib_load_index_stream( end_of_file, _Repo, _Root, _In ) :-
    !.
lib_load_index_stream( lib_index(Pn,Pa,_C,_D,Rel), Repo, Root, In ) :-
    !,
    directory_file_path( Root, Rel, Abs ),
    lib_load_assert( Pn, Pa, Repo, Abs ),
    read( In, Next ),
    lib_load_index_stream( Next, Repo, Root, In ).
lib_load_index_stream( Other, Repo, Root, In ) :-
    debug( lib, 'Non lib_index/5 term skipped: ~w', Other ),
    read( In, Next ),
    lib_load_index_stream( Next, Repo, Root, In ).

/** lib_load_assert( +Pn, +Pa, +Repo, +Abs ).

    Assert lib_index/4 clause for Pn/Pa, coming from Repo and absolute file Abs
    if one does not exist and throw error otherwise.

*/
lib_load_assert( Pn, Pa, Repo, Abs ) :-
    lib_tables:lib_index( Pn, Pa, Repo1, Abs1 ),
    !,
    lib_load_assert_existing( Pn, Pa, Repo1, Repo, Abs1, Abs ).
lib_load_assert( Pn, Pa, Repo, Abs ) :-
    asserta( lib_tables:lib_index( Pn, Pa, Repo, Abs ) ).

lib_load_assert_existing( _Pn, _Pa, Repo, Repo, Abs, Abs ) :-
    !. % maybe throw an internal error for debugging ? 
       % it should never get here
lib_load_assert_existing( Pn, Pa, Repo, Repo, Abs1, Abs ) :-
    Abs \== Abs1,
    write(single_lib_source_defines_multiple_predicate_version(Pn,Pa,Repo,Abs1,Abs)),nl,
    !.
    % throw(single_lib_source_defines_multiple_predicate_version(Pn,Pa,Repo,Abs1,Abs)).
lib_load_assert_existing( Pn, Pa, Repo1, Repo, Abs1, Abs ) :-
    current_prolog_flag( lib_multiple_distincts, false ),
    !,
    throw( lib_index_exists_for(Pn,Pa,Repo1,Abs1,Repo,Abs) ).
lib_load_assert_existing( Pn, Pa, _Repo1, Repo, _Abs1, Abs ) :-
    assert( lib_tables:lib_index(Pn,Pa,Repo,Abs) ).

lib_attach_filenames( false, _Root, _Repo, _Cxt ).
lib_attach_filenames( true, Root, Repo, Cxt ) :-
    lib_homonyms( Repo, Root ),
    lib_assert_attached_homonyms( Cxt ,Repo ).

lib_assert_attached_homonyms( Cxt ,Repo ) :-
    lib_tables:lib_attached_homonyms( Cxt, Repo ),
    !.
lib_assert_attached_homonyms( Cxt, Cxt ) :-
    !.  % fixme: should we be catching this earlier
lib_assert_attached_homonyms( Cxt, Repo ) :-
    assert( lib_tables:lib_attached_homonyms(Cxt,Repo) ).
