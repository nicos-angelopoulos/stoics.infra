
:- module( lib_pack_index, [lib_pack_index/0,lib_pack_index/1] ).

/** lib_pack_index.

Creates a src/LibIndex.pl with interface predicates for a pack.

*/
lib_pack_index :-
    exists_directory( prolog ),
    working_directory( Here, Here ),
    ( atom_concat(OverHere,'/',Here) ->
        true
        ;
        OverHere = Here
    ),
    directory_file_path( _, Pack, OverHere ),
    file_name_extension( Pack, pl, PackPl ),
    directory_file_path( prolog, PackPl, RelF ),
    !,
    lib_pack_index( RelF ).


/** lib_pack_index( +PackF ).

Load PackF and make an index of where do its exported predicates 
come from.

*/
lib_pack_index( RelPackF ) :-
    use_module( RelPackF ), 
    absolute_file_name( RelPackF, PackF ),
    directory_file_path( Dir, PackFile, PackF ),
    directory_file_path( Root, prolog, Dir ),
    file_name_extension( Pack, pl, PackFile ),
    directory_file_path( Root, src, SrcD ),
    directory_file_path( SrcD, 'LibIndex.pl', LibF ),
    ( exists_file(LibF) ->
        directory_file_path( SrcD, 'LibIndex.bku', BkuF ),
        copy_file(LibF,BkuF)
        ;
        true
    ),
    current_prolog_flag( allow_dot_in_atom, OldADA ),
    set_prolog_flag( allow_dot_in_atom, false ),
    open( LibF, write, Out ),
    findall( lib_index(Pa,Pn,any,Pack,File), ( 
                        predicate_property(Phead,imported_from(Pack)),
                        predicate_property(Phead,file(File)),
                        functor(Phead,Pa,Pn),
                        directory_file_path( SrcD, RelF, File ),
                        file_name_extension( RelStem, pl, RelF ),
                        portray_clause( Out, lib_index(Pa,Pn,any,Pack,RelStem) )
                    ),
                             _LITerms ),
    set_prolog_flag( allow_dot_in_atom, OldADA ),
    close( Out ).
