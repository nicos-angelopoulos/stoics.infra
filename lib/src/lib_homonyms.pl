% lib_homonyms( Repo ).
% 
% Make files from local directory available as homonyms in Repo
%

lib_homonyms( Repo ) :-
    compound( Repo ),
    !,  % we are within a cell of a pack
    Repo =.. [Pack,Cell],  % fixme: allow more complex terms
    prolog_load_context( directory, Root ),
    directory_file_path( Root, Cell, Coot ),
    atomic_list_concat( [Pack,Cell], '_', Mod ),
    lib_homonyms( Mod, Coot ).

lib_homonyms( Repo ) :-
    prolog_load_context( directory, Root ),
    lib_homonyms( Repo, Root ).

lib_homonyms( Repo, _Root ) :-
    Repo \== user,
    lib_tables:lib_repo_homonyms( Repo, Root ),
    !,
    % fixme: check for errors ?
    Mess = 'Homonyms already loaded for repo: ~w, from source dir: ~w',
    debug( lib, Mess, [Repo,Root] ).
lib_homonyms( Repo, Root ) :-
    directory_file_path( Parent, Base, Root ),
    lib_homonyms_dir( Base, Parent, Root, Repo ),
    asserta( lib_tables:lib_repo_homonyms(Repo,Root) ).

lib_homonyms_dir( prolog, Parent, _FullD, Repo ) :-
    !,
    directory_homonyms_src( Parent, Repo ).
lib_homonyms_dir( _Base, _Parent, FullD, Repo ) :-
    directory_homonyms_src( FullD, Repo ).

directory_homonyms_src( TopD, Repo ) :-
    directory_file_path( TopD, src, SrcD ),
    exists_directory( SrcD ),
    !,
    lib_dir_homonyms_load( Repo, SrcD ).
directory_homonyms_src( TopD, Repo ) :-
    lib_dir_homonyms_load( Repo, TopD ).

lib_dir_homonyms_load( Repo, Root ) :-
    lib_dir_contents( Root, Files, Dirs ),
    maplist( lib_assert_homonym(Repo), Files ),
    lib_dir_homonyms_load_cont( Dirs, Repo ).

lib_dir_homonyms_load_cont( [], _Repo ) :-
    !.
lib_dir_homonyms_load_cont( Dirs, Repo ) :-
    maplist( lib_dir_homonyms_load(Repo), Dirs ).

lib_assert_homonym( Repo, File ) :-
    file_base_name( File, Base ),
    file_name_extension( Stem, pl, Base ),
    !,
    lib_assert_homonym_stem( Stem, Repo, File ).
lib_assert_homonym( _Repo, _File ).

lib_assert_homonym_stem( 'LibIndex', _Repo, _File ) :-
    !.
lib_assert_homonym_stem( Stem, Repo, File ) :-
    assert( lib_tables:lib_homonym(Stem,Repo,File) ).
