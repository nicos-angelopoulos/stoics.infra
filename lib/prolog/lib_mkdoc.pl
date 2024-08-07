:- module( lib_mkdoc, [lib_mkdoc/0,lib_mkdoc/1] ).

:- dynamic( lib_mkdoc:loaded/1 ).
:- dynamic( lib_mkdoc:lib_index/5 ).

lib_mkdoc :- 
    lib_mkdoc( '.' ).

/** lib_mkdoc( +Dir ).

Make docs for lazy packs.

:- asserta( library_directory('/usr/local/users/nicos/local/git/lib/swipl-7.5.1/pack/lib/prolog/mkdoc') ).
:- use_module( '/usr/local/users/nicos/local/git/lib/swipl-7.5.1/pack/lib/prolog/mkdoc/lib' ).

*/
lib_mkdoc( DirIn ) :-
    absolute_file_name( DirIn, Dir, [file_type(directory)] ),
    working_directory( Old, Dir ),
    directory_file_path( _LibRoot, Pack, Dir ),
    directory_file_path( Dir, prolog, PrologD ),
    file_name_extension( Pack, pl, PackF ),
    directory_file_path( PrologD, PackF, AbsPackF ),
    %
    directory_file_path( doc, PackF, DocF ),
    % shell( 'ln -s ../src doc/src' ),
    open( DocF, write, Out ),
    open( AbsPackF, read, In ),
    read( In, (:- module(Pack,Exports)) ),
    portray_clause( Out, (:- module(Pack,Exports)) ),
    directory_file_path( Dir, src, SrcD ),
    directory_file_path( SrcD, 'LibIndex.pl', SrcLib ),
    ( exists_file(SrcLib) -> 
        ensure_loaded( lib_mkdoc:SrcLib ),
        listing( lib_mkdoc:lib_index/5 )
        ;
        true
    ),
    set_prolog_flag( verbose_load, full ),
    % asserta( lib_mkdoc:loc(Dir,Out) ),
    absolute_file_name( pack('lib/prolog/mkdoc'), Mkdoc, [file_type(directory)] ),
    % asserta( library_directory(Mkdoc) ),
    % use_module( library(lib) ),
    % absolute_file_name( pack('lib/prolog/mkdoc/lib.pl'), LibPl, [file_errors(fail),access(exist)] ),
    % use_module( LibPl ),
    % export( lib_mkdoc:lib/1 ),
    % import( Pack:lib/1 ),
    % spy( lib:lib/1 ),
    % ensure_loaded( AbsPackF ),
    nl( Out ),
    portray_clause( Out, (:- assert(user:library_directory(Mkdoc))) ),
    directory_file_path( Mkdoc, lib, MkLib ),
    portray_clause( Out, (:- use_module(MkLib)) ),
    portray_clause( Out, (:- assert(lib:doc_module(Pack))) ),
    nl( Out ),
    %  :- use_module( '/usr/local/users/nicos/local/git/lib/swipl-7.5.1/pack/lib/prolog/mkdoc/lib' ).
    nl( Out ),
    read( In, Term ),
    lib_mkdoc_stream( Term, In, Locals ),
    % lib_mkdoc_stream( Term, In, Out ),
    close( In ),
    % close( Out ),

    % open( DocF, append, OutA ),    % make sure we get the docs in the file too
    open( AbsPackF, read, InA ),
    read( InA, _ModDfn ),
    copy_stream_data( InA, Out ),
    close( InA ),
    % close( OutA ),
    lib_mkdoc_stream_locals( Locals, Out ),
    nl( Out ), portray_clause( Out, (:- retractall(lib:doc_module(_))) ), nl( Out ),
    close( Out ),

    catch( ensure_loaded(DocF), Caught, (write(caught(Caught)),nl) ),
    doc_save( DocF, [doc_root('doc/html')] ),
    %
    working_directory( _, Old ).

lib_mkdoc_stream( end_of_file, _In, [] ).
lib_mkdoc_stream( Term, In, Locs ) :-
     ( Term = (:- lib(Pn/Pa)) -> Locs = [Pn/Pa|Ls] ; Locs = Ls ),
     catch( read( In, Next ), _, Next = errored ),
     lib_mkdoc_stream( Next, In, Ls ).


lib_mkdoc_stream_locals( [], _Out ).
lib_mkdoc_stream_locals( [Pn/Pa|T], Out ) :-
    lib_mkdoc:lib_index( Pn, Pa, _, _, File ),
    directory_file_path( src, File, SrcFile ),
    file_name_extension( SrcFile, pl, RelF ),
    exists_file( RelF ),
    lib_mkdoc_file( RelF, Out ),
    !,
    lib_mkdoc_stream_locals( T, Out ).
lib_mkdoc_stream_locals( [H|T], Out ) :-
    write( failed_to_locate_sources_for_local(H) ), nl,
    lib_mkdoc_stream_locals( T, Out ).
    
lib_mkdoc_file( RelF, _Out ) :-
    lib_mkdoc:loaded( RelF ),
    !.
lib_mkdoc_file( RelF, Out ) :-
    open( RelF, read, RelIn ),
    copy_stream_data( RelIn, Out ),
    close( RelIn ),
    asserta( lib_mkdoc:loaded( RelF ) ).
