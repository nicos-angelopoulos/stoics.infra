:- module( lib, [lib/1,lib/2] ).

% :- dynamic( lib_mkdoc:lib_index/5 ).
lib( _, _ ). % fixme

lib( Lib ) :-
    catch( use_module(library(Lib)), _, fail ),
    !.
/*
lib( Pn/Pa ) :-
    lib_mkdoc:lib_index(Pn,Pa,_,_,File),
    !,
    lib_mkdoc:loc(Dir,Out),
    directory_file_path( Dir, File, AbsStem ),
    file_name_extension( AbsStem, pl, AbsF ),
    open( AbsF, read, In ),
    copy_stream_data( In, Out ),
    close( In ),
    nl( Out ).
    */
lib( _ ).
