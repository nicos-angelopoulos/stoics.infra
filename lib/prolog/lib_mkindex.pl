:- module( lib_mkindex, [lib_mkindex/2,lib/1,lib/2] ).

:- use_module( library(lib) ).

:- dynamic( lib_mkindex_defines:defines/2 ).

% lib( A, _B ) :- lib( A ).
lib( _A, _B ).
lib( defines(Pn/Pa) ) :- !, assert( lib_mkindex_defines:defines(Pn,Pa) ).
lib(SysLib) :- atomic(SysLib), lib:lib_loading_context( Cxt ), catch(ensure_loaded(Cxt:library(SysLib)),_,true).
% lib(_). 
% lib_suggests( _ ).
% lib_init( _ ).

/** lib_mkindex( Repo, Opts ).

    Make 

Opts (can be unlistted singleton
 * load(Load=true)
   set to false to stop loading of the main file (
 * homonyms(Hnms=false)
   set to true to only index homonyms

@author nicos angelopoulos
@version  0.1 2017/3/2

*/
lib_mkindex( Repo, ArgS) :-
    ( is_list(ArgS) -> ArgS = Args; Args = [ArgS] ),
    % en_list( ArgS, Args ),
    % Defs = [homonyms(false),load(false)],
    Defs = [homonyms(false),load(true)],
    append( Args, Defs, Opts ),
    lib:lib_type( Repo, Type, Mod, Root, Load ),
    debug( lib, 'Mkindex for: ~w, of type: ~w, mod: ~w, load: ~w, root: ~w', [Repo,Type,Mod,Load,Root] ),
    lib_mkindex( Type, Repo, Mod, Root, Load, Opts ).
    % fixme: unload_file ???

lib_mkindex( lib, Repo, Mod, Root, Load, Opts ) :-
    Setup = asserta( lib_tables:lib_context(Repo,Root) ),
    Goal  = lib:lib_load_file( Load, Repo, Opts ),
    Clean = ( once(retract(lib_tables:lib_context(Repo,Root))) ),
    setup_call_cleanup(Setup, Goal, Clean),
    memberchk( homonyms(Hmns), Opts ),
    lib_mkindex_dir( Mod, Hmns, Root, '', Pairs ),
    directory_file_path( Root, 'LibIndex.pl', LibIndex ),
    mk_index_portray_clauses( Pairs, file(LibIndex) ),
    debug( lib, 'Wrote on file: ~w', LibIndex ).

lib_mkindex_dir( Repo, Hmns, Root, Sub, Sort ) :-
    directory_file_path( Root, Sub, Full ),
    lib:lib_dir_contents( Full, Files, Dirs ),
    maplist( lib_mkindex_dir(Repo,Hmns,Full), Dirs, Dest ),
    maplist( lib_mkindex_file(Repo,Root,Hmns), Files, Fest ),
    flatten( [Dest,Fest], Pairs ),
    sort( Pairs, Sort ).

lib_mkindex_file( Repo, Root, Hmns, AbsF, [Sourced,Asserted] ) :-
    directory_file_path( _TheDir, File, AbsF ),
    file_name_extension( Stem, pl, File ),
    Stem \== 'LibIndex',
    !,
    trace,
    % ensure_loaded( Repo:AbsF ),
    % findall( Pn/Pa-AbsF, () ).
    % findall( lib_index(Pn,Pa,any,user,AbsF),  )
    directory_file_path( Root, RelF, AbsF ),
    file_name_extension( RelS, pl, RelF ),
    findall( lib_index(Pn,Pa,any,user,RelS), 
                (
                    source_file(Repo:Head,AbsF),
                    functor(Head,Pn,Pa),
                    lib_mkindex_homonym(Hmns,Stem,Pn)
                ),
                    Sourced ),
    findall( lib_index(Pn,Pa,any,user,RelS),
                lib_mkindex_defines:defines(Pn,Pa)
                , 
                Asserted ),
    retractall( lib_mkindex_defines:defines(_,_) ).
    % unload_file( AbsF ).
    % fixme: unload_file ???
lib_mkindex_file( _Repo, _Root, _Hmns, _File, [] ).

lib_mkindex_homonym( true, Stem, Stem ).
lib_mkindex_homonym( false, _, _ ).

mk_index_portray_clauses( Pairs, file(LibIndex) ) :-
    open( LibIndex, write, Out ),
    maplist( portray_clause(Out), Pairs ),
    close( Out ).
