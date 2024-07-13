:- module( lib, [lib/1,lib/2] ).

:- dynamic( lib:doc_module/1 ).

% :- dynamic( lib_mkdoc:lib_index/5 ).
lib(_A,_B).
% lib( A, B ) :- write(a(A)-b(B)), nl.
     % fixme

lib( Lib ) :-
    prolog_load_context( module, Lod ),
    ( Lib = Pred/Ar -> 
          ( current_predicate(Lod:Pred/Ar) ->
                    true
                    ;
                    ( lib:doc_module(Lod) ->
                         true
                         ;
                         functor(Goal,Pred,Ar),
                         Lod:assert(Goal)
                    )
          )
          ;
          catch( Lod:use_module(library(Lib)), _, fail )
    ),
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
