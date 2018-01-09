:- module( lib, [
                    lib/1, lib/2   % +Repo[, +Opts]
        ] ).

                     % lib_suggests/1,  % fixme: feature()
                     % lib_promise/2,
                     % lib_expects/1+2,
                     % lib_init/1

:- ensure_loaded( library(prolog_pack) ).
    % query_pack_server/3

:- ensure_loaded( '../src/lib_init'  ).
:- ensure_loaded( '../src/lib_load'  ).
:- ensure_loaded( '../src/lib_type'  ).
:- ensure_loaded( '../src/lib_auxil' ).
:- ensure_loaded( '../src/lib_attach').
:- ensure_loaded( '../src/lib_homonyms'  ).
:- ensure_loaded( '../src/lib_suggests'  ).
:- ensure_loaded( '../src/lib_expects'  ).
:- ensure_loaded( '../src/lib_promise'  ).
:- ensure_loaded( '../src/lib_message'  ).


:- dynamic( lib_tables:lib_repo/4 ).             % +Repo, +Type, +Root, +Load 
:- dynamic( lib_tables:lib_repo_index/2 ).       % +Repo, +IdxFile
:- dynamic( lib_tables:lib_repo_homonyms/2 ).    % +Repo, +SrcDir
:- dynamic( lib_tables:lib_context/2 ).          % +Ctx, +Root
:- dynamic( lib_tables:lib_index/4 ).            % +Pa, +Pn, +Repo, +File. records loaded indices
:- dynamic( lib_tables:lib_promise/2 ).          % +Load, +Pid.  hot swap Pid with loading Load
:- dynamic( lib_tables:lib_homonym/3 ).          % +Stem, +Repo, +File. record loaded homonym
:- dynamic( lib_tables:lib_loaded_index/2 ).     % +Repo, +File. tracks loaded index files
:- dynamic( lib_tables:lib_loaded_homonyms/2 ).  % 
:- dynamic( lib_tables:lib_attached_indices/2 ). % +Ctx, Repo
:- dynamic( lib_tables:lib_attached_homonyms/2 ).% +Ctx, Repo
:- dynamic( lib_tables:lib_lazy/1 ).             % +Repo
:- dynamic( lib_tables:lib_full/2 ).             % +Repo
:- dynamic( lib_tables:lib_packs_at/2 ).         % +Repo, +Dir


% fixme: user defined ones
lib_src_sub_dir( src ).
lib_src_sub_dir( 'src/lib' ).
lib_src_sub_dir( 'src/auxil' ).

/** <module> Predicate based code development.

 This pack implements methods for loading code into SWI Prolog programs.
 One of the major innovations the library introduces, is that of progressive, lazy loading of packs.
 That is, if only a specific predicate is (lazily) required
 from a =|pack(lib)-aware|= pack, only that and its dependent code will be loaded.

That is, your code can load things like
==
?- lib( stoics_lib:kv_compose/3 ).
?- lib( stoics_lib:kv_decompose/3 ).
==
and only the relevant parts of the pack(stoics_lib) will be loaded.

If later on your code decides to do a
==
?- lib(stoics_lib). 
==
The remainder of the library loads up quietly and politely. 


Please note that this is, at top level at least, orthogonal to any other loading. 

You can still do 
==
?- use_module( library(stoics_lib) ).
==
and get the whole thing into memory.

Pack(lib) plays reasonably well with the documentation server. Bar, the normal
limitations of the server.  By convention and to help locating the module docs,
lazy packs should define (Pack)/0 predicate in same file as the mods docs.
Searching for that on doc server, should make it easy enough to get to it.

 Although this library, _pack(lib)_, contains a number of involved features
 it can also be used as a straight forward shorthand, 
 replacement for use_module(library(Lib)).

 ==
 ?- lib(Atomic).
 ==
 is equivelant to =|use_module(library(Atomic))|= if Atomic is a system library
 or an installed Pack, while it will interogate the SWI pack server 
 for matching packs if Atomic is atomic and not an installed pack.

In addition the library allows for loading with initializations turned off.

A good example of how to create a lazy pack is
pack(stoics_lib), http://stoics.org.uk/~nicos/sware/stoics_lib
v0.3. An example of how to lazy load things from stoics_lib
is the latest pack(debug_call) http://stoics.org.uk/~nicos/sware/debug_call
v0.4.

---++ Repositories

Code is managed in _repositories_ (also _repo_) that can be either packs or libs. 

A _pack_ is a unit of programs as managed by built-in SWI package manager 
(=|library(prolog_pack)|=). A _lib_ (_library_) is a directory containing a
number of program files.

pack(lib) supports a number of ways to organise your code and load it, but
it comes to its own when code s organised as predicate-in-a-file fashion.
In this mode of development a predicate such as kv_decompose/3
would be defined on file kv_decompose.pl which will only containing code for defining
this predicate, with the possible exception of helper predicates that are
too specific to be of outside interest.
==
kv_decompose( [], [], [] ).
kv_decompose( [K-V|T], [K|Tk], [V|Tv] ) :-
	kv_decompose( T, Tk, Tv ).
==

Lib code is considered as coming from the special pack _user_.

---++ Code-tables

Associated with each repository are 2 types of code-tables: (code-)_indices_ and (file-)_locators_.

A code-index maps a predicate identifier along with its source repo to
an absolute file name of the source that defines it.
Indices are of the form:

==
lib_tables:lib_index( Pname, Parity, Repo, AbsFile ).
==

File locators store all filenames in a repository. 
These can be named matched predicate names that need to be loaded.
pack(lib) can be directed to assume that files from a specific 
repository exhibit this _homonyms_ property.
Locators are of the form:
==
lib_tables:lib_homonym( Stem, Repo, AbsFile ).
==

For each index file loaded for a repository, the following is asserted:
==
lib_tables:lib_loaded_indices(Repo,File)
==

and for each locator 
==
lib_tables:lib_loaded_homonyms(Repo,Stem,File)
==

When loading a repository the user can choose whether to load
indices and locators independently.

---++ Loading source code

During the process of loading code into memory, lib/1 and /2 directives are used
to locate code to which the specific code depends. 

There are three main categories of operations: 
  * load code from explicitly identified repository
  * attach a repository for implicit loading
  * load code from attached repositories

These operations are all specific to the loading context. This is achieved by 
creating meta-predicates that identify which part of the repository base each
context has access to.

Attachment of repository is registered via =|lib_tables:lib_attached_indices(To,PackIG)|=
and =|lib_tables:lib_attached_homonyms(To,PackFG)|=.
==
lib_tables:lib_attached_indices(bims,options).
lib_tables:lib_attached_homonyms(bims,false).
==
attaches the indices but not the file locators.

Since all code from directory-libs load to a single module (_user_), loading 
code has either access to all such code, or to none.

---++ Conventions

Packs are expected to have matching top directories and main files. The main file
of a pack should be within top directory _prolog/_. (The directory convention 
is set by library(prolog_pack)). For example for pack _bims_ the following file should exist 
in packs directory:

==
bims/prolog/bims.pl
==

For packs the main code directory is _src/_. Additionally _src/lib_ and _src/auxil_
are treated as code directories.

---++ Internals

Variables
  * Repo
    repository
  * Pack
    prolog pack (installed or locally addressed)
  * Lib
    directory containing code 
  * Root 
    absolute reference to the root directory of a Repo
  * Pn 
    predicate name
  * Idx 
    library index term
  * Hmn
    library file homonym term
  * Pa 
    predicate arity
  * Cxt 
    context module

Predicate names
  * index
  * file

---++ Pack info

This is a complete re-write of pack(requires) v1.1.

Listens to =|debug(lib)|=.

@author nicos angelopoulos
@version  1.0 2017/3/6
@version  1.1 2017/3/9,  lazy loading
@version  1.2 2017/3/11, fixed missing cut, added lib(version(V,D))
@version  1.3+4 2017/8/8, fixed multi-source for user, improved contact to server, install while lazy loading
@version  1.5 2017/8/15
@see http://stoics.org.uk/~nicos/sware/lib

*/

lib_defaults( pack, [load(true),index(true),homonym(true),type(pack),mode(self)] ).
lib_defaults( lib, [load(false),index(true),homonym(true),type(lib),mode(self)] ).

/**  lib( +Operand ).
     lib( +Operand, +Opts )

Loads code or/and indices of Repo into the current context.

When Repo =|homonym(Repository)|= then only the homonims of local dir
(adjusted for pack dir structure) are added to as coming from Repository.

Operands
  * SysLibrary
    An installed library (atomic). Is loaded with use_module( library(SysLibrary) ).
  * Pack
    A pack known to SWI. If pack is not installed then the server is contacted to look
    for name-matching packs that can be installed. If there is at least
    one matching pack, it can be installed interactively.
  * LibDir
    Declares a library directory should be 
      * atomic,       
           an absolute path
      * rel(Rel)     
           Rel will be absolute_file_name/3 to be made into an absolute location
      * alias(Dir)    
           as above, but ensures that nothing in Dir is interpreted as a command
      * compound 
           compound terms are tried to be expanded to an existing directory

    The default options for libs are
      * load(Load=false)
      * index(Idx=true)
      * homonyms(Hmns=true)
      * type(Type=lib)

  * Command
    One of 
    * homonyms(From)
       attach homonyms From pack
    * init(Lib)
    * init(Lib,Cxt)
       declare initilization call (library can be loaded without this firing, if so needed, as is the
       case for lib_mkindex/1)
    * suggests(Lib)
       it is likely you need Lib for full functionalilty
    * promise(Pred,Load)
       Pred is needed for functionality and it can be found by loading Load, but it will only happen at Pred's first call
    * expects(Pid,Mess)
    * expects(Pid,Mess,Call)
       complains if Pid is not defined at loading time. Mess should be a debug style message
       with one ~w which will be called with Pid as its printing argument. 
       If call is present, is called after the printing of the message.
    * version(Vers,Date)
       return version and publication date
    
Opts
  * index(Idx)
    whether to load indices

  * homonym(Hnym)
    whether to load homonym file-locators

  * load(Load)
    whether to load the main entry point of Repo

  * type(Type)
    enforce a particular type of repository (pack or lib)
  
The defaults depend on whether Repo is a pack or a lib. 
* Packs get defaults
    [load(true),index(true),homonym(false),type(pack)]
* Libs get defaults
    [load(false),index(true),homonym(true),type(lib)]

When invvoked with code attaching operands (SysLibrary, Pack or Lib) 
the predicate will first load anything that needs to be loaded in their native module
and then import predicates from that module. Attaching a lib or pack means that the 
predicates pointed to by indices and by file name from the target pack/lib become
available to the importee. Option =|index(Idx)|= controls whether =LibIndex.pl=
based indices are attahced whereas =|homonym(Hmns)|= control the attachment of
the file names from the target.

For example to only import the interface predicates of pack =ex1= use
==
?- lib(ex1, [type(pack),load(true),index(false),homonym(false)]).
==

Assume that ex1 is a pack that is not installed on your Prolog installation, but you have its sources
unpacked on local dir =|/tmp/ex1/|= you can load it interface predicates with:

For example to only import the interface predicates of pack =ex1= use
==
?- lib('/tmp/ex1', [type(pack),load(true),index(false),homonym(false)]).
==

Assume there is a file =|src/lib/foo.pl|= in =ex1= defining predicate foo/1, then you can load its code with
==
?- lib('/tmp/ex1', [type(pack),load(true),index(false),homonym(true)]).
?- lib(foo/1).
==
The above will first load foo.pl (by means of matching its filename to the predicate name) into ex1:
and then assuming that this loaded ex1:foo/1 it will import it into current context (here this is =user+).

Assuming foo.pl also defines predicate bar/2 and there is a file =|src/LibIndex.pl|= within =ex1= containing the
line
==
lib_index( bar, 2, swipl(_), user, 'lib/foo.pl' ). 
==
Then the code for foo_bar/2 can be loaded with
==
?- lib('/tmp/ex1', [type(pack),load(true),index(true),homonym(false)]).
?- lib(bar/2).
==


Pack lib can be used to create and access *skeleton* packs. These packs, may load very little interface
code but their code base can be loaded on demand and piece-meal. That is if a specific non-interface
predicate is required, it will be located and loaded along with all its dependencies.

An example of such a pack is =|stoics_lib|=. 
The following commands: 1. load the minimal interface,,
 2, load the code for a specific non-interface predicate.
==
?- lib(stoics_lib).
?- lib(kv_decompose/3).
==

The above two directives can be shortened to:
==
?- lib(stoics_lib:kv_decompose/3).
==

@tbd when predicate is missing from stoics_lib while loading from b_real, we get clash between main and lazy, error should be clearer (the pred select_all/3 was actually not defined in file either)

*/
lib( Repo ) :-
    % fixme: add alias() command
    lib( Repo, [] ).

lib( Repo, ArgS ) :-
    lib_loading_context( Cxt ),
    lib_en_list( ArgS, Args ),
    lib( Repo, Cxt, Args ).

lib( Lib, Cxt, Args ) :-
    debug( lib, 'lib directive: ~w, in context: ~w, with opts: ~w', [Lib,Cxt,Args] ),
    fail.
lib( Pn/Pa, Cxt, Args ) :-
    !,
    lib_load( Cxt, Pn, Pa, Args ).
lib( Repo:Pn/Pa, Cxt, Args ) :-
    !,
    lib_explicit( Repo, Pn, Pa, Cxt, Args ).
lib( homonyms(Repo), _, _Args )  :-   
                  % load local homonyms as coming from Repo. can be added to
                  % pack to indicate that LibIndex is incomplete or missing 
                  % by default packs do not load their homonyms
    !,
    lib_homonyms( Repo ).
lib( source(Src), _Cxt, Opts ) :-
    !,
    lib_source( Src, Opts ).
lib( end(Src), _Cxt, Opts ) :-
    lib_source_end( Src, Opts ).

% lib( alias(Alias), Cxt, Opts ) :-
    % !,
    % lib_alias( Alias, Cxt, Opts ).
lib( version(V,D), _, _Args ) :-
    !,
    % V = 1:2, D = date(2017,3,11).
    % V = 1:4:0, D = date(2017,8,8).
    V = 1:5:0, D = date(2017,8,15).
lib( suggests(Lib), _, _Args ) :-  % fixme: add note() option
    !,
    lib_suggests( Lib ).
lib( promise(PidS,Load), _, _Args ) :-  % fixme: add note() option
    !,
    lib_promise( PidS, Load ).
lib( expects(Lib,Mess), _, _Opts ) :-  % fixme: add note() option 
    !,
    lib_expects( Lib, Mess ).
lib( expects(Lib,Mess,Goal), _, _Opts ) :-  % fixme: add note() option 
    !,
    lib_expects( Lib, Mess, Goal ).
lib( init(Lib), Cxt, _Opts ) :-
    !,
    lib_init( Lib, Cxt ).
lib( sys(SysLib), Cxt, _Opts ) :-
    !,
    % AbsOpts = [access(read),file_errors(fail),file_type(prolog)],
    % absolute_file_name(library(SysLib), AbsLib, AbsOpts ),
    absolute_file_name(library(SysLib), AbsLib ),
    % fixme: need map from SysLib -> Repo
    lib_retract_lazy( SysLib, WasLazy ),
    lib_sys_lazy( WasLazy, SysLib, AbsLib, ', expected,', Cxt ).

lib( Repo, Cxt, Opts ) :-
    lib_tables:lib_lazy( Repo ),
    !,
    lib_lazy_no_more( Repo, Cxt, Opts ).
lib( Repo, Cxt, _Args ) :-
    lib_tables:lib_packs_at( Cxt, PrivPacksD ),
    directory_file_path( PrivPacksD, Repo, PackRoot ),
    directory_file_path( PackRoot, prolog, PackPrologD ),
    directory_file_path( PackPrologD, Repo, PrologStem ),
    file_name_extension( PrologStem, pl, PlF ),
    exists_file( PlF ),
    !,
    debug( lib, 'Loading from private pack with entry point: ~p', PlF ),
    % ensure_loaded( PlF ).
    lib_defaults( pack, PackLoadDefs ),
    lib( Repo, PackRoot, PlF, Cxt, PackLoadDefs ).
lib( Repo, Cxt, Args ) :-
    lib_type( Repo, RepoType, RepoMod, RepoRoot, RepoLoad ),
    !,
    lib_reg_repo( RepoMod, RepoType, RepoRoot, RepoLoad ),
    MsId = 'Identified repo: ~w as: ~w, loading in: ~w, with root: ~w',
    debug( lib, MsId, [Repo,RepoType,RepoMod,RepoRoot] ),
    lib_defaults( RepoType, Defs ),
    append( Args, Defs, Opts ),
    lib( RepoMod, RepoRoot, RepoLoad, Cxt, Opts ).
lib( SysLib, Cxt, _Args ) :-
    AbsOpts = [access(read),file_errors(fail),file_type(prolog)],
    absolute_file_name(library(SysLib), AbsLib, AbsOpts ),
    lib_retract_lazy( SysLib, WasLazy ),
    lib_sys_lazy( WasLazy, SysLib, AbsLib, '', Cxt ),
    !.  % fixme: is this too late in the body?
    
    /*
    Assert = asserta( lib_tables:lib_full(SysLib,AbsLib) ),
    Goal = Cxt:use_module(library(SysLib)),
    Retract = retract(lib_tables:lib_full(SysLib,AbsLib) ),
    setup_call_cleanup(Assert, Goal, Retract),
    % catch( Cxt:use_module(library(SysLib)), _, fail ),
    !,
    debug( lib, 'System library: ~w, loaded in: ~w', [SysLib,Cxt] ).
    */
lib( Repo, Cxt, Args ) :-
    lib_tables:lib_repo(Repo,Type,Root,Load),
    !,
    lib_repo( Repo, Type, Root, Load, Cxt, Args ).
lib( Root, Cxt, Args ) :-
    lib_tables:lib_repo(Repo,Type,Root,Load),
    !,
    lib_repo( Repo, Type, Root, Load, Cxt, Args ).
lib( Pack, Cxt, Opts ) :-
    lib_missing( Pack, Cxt, Opts, true ),
    !.
lib( Repo, Cxt, Opts ) :-
    memberchk( mode(Mode), Opts ),
    lib_not_found( Mode, Repo, Cxt ).

lib_retract_lazy( SysLib, WasLazy ) :-
    lib_tables:lib_lazy(SysLib),
    !,
    WasLazy = true.
lib_retract_lazy( _SysLib, false ).

lib_sys_lazy( _, SysLib, AbsLib, ExplicitTkn, Cxt ) :-
    lib_sys( SysLib, AbsLib, ExplicitTkn, Cxt ),
    !.   % don't need to reassert it as it is now fully loaded
lib_sys_lazy( true, SysLib, _AbsLib, _ExplicitTkn, _Cxt ) :-
    asserta( lib_tables:lib_lazy(SysLib) ),
    fail.

lib_sys( SysLib, AbsLib, ExplicitTkn, Cxt  ) :-
    Assert = asserta( lib_tables:lib_full(SysLib,AbsLib) ),
    Goal = Cxt:use_module(library(SysLib)),
    Retract = retract(lib_tables:lib_full(SysLib,AbsLib) ),
    setup_call_cleanup(Assert, Goal, Retract),
    debug( lib, 'System~w library: ~w, loaded in: ~w', [ExplicitTkn,SysLib,Cxt] ).

lib_not_found( self, Repo, _Cxt ) :-
    Mess = 'Failed to locate repository:~w, (no local lib, local pack or remote pack)',
    lib_message_report( Mess, [Repo], informational ).
lib_not_found( suggests, Repo, _Cxt ) :-
    Mess = 'Failed to locate suggested repository:~w, (no local lib, local pack or remote pack)',
    lib_message_report( Mess, [Repo], informational ).

lib_explicit( Repo, Pn, Pa, Cxt, _Opts ) :-
    lib_tables:lib_full(Repo,_),
    !,   % this should be able to cope with cyclic dependencies? 
         % check with options
    Cxt:import( Repo:Pn/Pa ).
lib_explicit( Repo, Pn, Pa, Cxt, _Opts ) :-
    current_predicate( Repo:Pn/Pa ),
    !,
    lib_import_existing( Repo, Pn/Pa, Cxt ).
lib_explicit( Repo, Pn, Pa, Cxt, Opts ) :-
    lib_type( Repo, Type, Rmod, Root, Load ),
    lib_repo_lazy_assert( Rmod ),
    lib_explicit_repo( Type, Repo, Rmod, Root, Load, Pn, Pa, Cxt, Opts ),
    !.
lib_explicit( Repo, Pn, Pa, Cxt, Opts ) :-
    lib_missing( Repo, Cxt, Opts, false ),
    !,
    lib_explicit( Repo, Pn, Pa, Cxt, Opts ).
lib_explicit( Repo, Pn, Pa, Cxt, _Args ) :-
    % 17.03.24; the following 2 lines create a cycle
    % lib( Repo, Cxt, Args ),
    % lib( Pn/Pa, Cxt, [repo(Repo)|Args] ).
    Mess = 'Failed to locate: ~w within explicit repository:~w, within context: ~w',
    lib_message_report( Mess, [Pn/Pa,Repo,Cxt], error ).

lib_explicit_repo( pack, Repo, Rmod, Root, Load, Pn, Pa, Cxt, Opts ) :-
    file_name_extension( LoadStem, pl, Load ),
    atomic_concat( LoadStem, '_lazy', LazyStem ),
    file_name_extension( LazyStem, pl, LazyF ),
    ( exists_file(LazyF) ->
        true
        ; 
        Mess = 'Lazy loading file: ~w does not exist (context: ~w)',
        lib_message_report( Mess, [LazyF,Cxt], informational),
        fail
    ),
    lib_defaults( pack, Defs ),
    append( Opts, Defs, All ),
    lib( Rmod, Root, LazyF, Cxt, All ),
    % ensure_loaded( Rmod:LazyF ),
    % lib_reg_repo( Repo, pack, Root, LazyF ),
    lib( Pn/Pa, Cxt, [repo(Repo)|Opts] ).

lib_repo_lazy_assert( Repo ) :-
    lib_tables:lib_lazy( Repo ),
    !.
lib_repo_lazy_assert( Repo ) :-
    asserta( lib_tables:lib_lazy(Repo) ).

lib_missing( Pack, Cxt, Args, Load ) :-
	prolog_pack:confirm( contact_server(Pack), yes, [] ),
    G = query_pack_server(search(Pack), Result, [] ),
    catch( prolog_pack:G, _Ball, fail ),
    Result \== false,
    lib_defaults( lib, LibDefs ),
    append( Args, LibDefs, Opts ),
    memberchk( mode(Mode), Opts ),
    catch( prolog_pack:pack_list(Pack), _, fail ),
	prolog_pack:confirm( pack_on_server(Mode,Pack), yes, [] ),
	!,
	pack_install( Pack ),
    lib_missing_load( Load, Cxt, Pack ).

lib_missing_load( true, Cxt, Pack ) :-
	Cxt:use_module( library(Pack) ).
lib_missing_load( false, _Cxt, _Pack ).

lib_import_existing( Repo, Pn/Pa, Cxt ) :-
    functor( Phead, Pn, Pa ),
    predicate_property(Repo:Phead,exported),
    !,
    debug( lib, 'Importing from existing : ~w, into: ~w', [Repo:Pn/Pa,Cxt] ),
    Cxt:import( Repo:Pn/Pa ).
lib_import_existing( Repo, Pn/Pa, Cxt ) :-
    functor( Phead, Pn, Pa ),
    predicate_property(Repo:Phead,imported_from(Mod)),
    !,
    debug( lib, 'Importing from parent: ~w, via: ~w, pred: ~w, and context: ~w', [Mod,Repo:Pn/Pa,Cxt] ),
    Mod:import(Repo:Pn/Pa).
lib_import_existing( Repo, Pn/Pa, Cxt ) :-
    debug( lib, 'Exporting on: ~w and then importing:~w, into: ~w', [Repo,Pn/Pa,Cxt] ),
    export( Repo:Pn/Pa ),
    Cxt:import( Repo:Pn/Pa ).

lib_lazy_no_more( Repo, Cxt, Opts ) :-
    % use_module( Repo:library(Repo) ).
    lib_type( Repo, Type, _RepoMod, _RepoRoot, RepoLoad ),
    open( RepoLoad, read, In ),
    read( In, ModuleDfn ), 
    ModuleDfn = (:- module(Repo,Exports) ),
    lib_defaults( Type, Defs ),
    append( Opts, Defs, All ),
    maplist( lib_explicit_exports(Repo,Cxt,All), Exports ),
    close( In ),
    !.
lib_lazy_no_more( Repo, Cxt, Opts ) :-
    throw( failed_to_unset_lazy_mode_for(Repo,Cxt,Opts) ).

lib_explicit_exports( Repo, Cxt, Opts, Pn/Pa) :-
    lib_explicit( Repo, Pn, Pa, Cxt, Opts ).

% lib_alias( Alias, Cxt, Opts ) :-
    % !,
    % absolute_file_name( Alias, Dir, [access(exist)] ),
    % lib( Dir, Cxt, Opts ).
% lib_alias( Alias, _Cxt, _Opts ) :-
    % throw( alias_does_not_correspont_to_lib(Alias) ).

lib( Repo, Root, Load, Cxt, Opts ) :-
    ( catch(lib_load_repo_root_index_file(Repo,Root), _, true ) -> true; true ),
    Setup = asserta( lib_tables:lib_context(Repo,Root) ),
    Goal  = lib_load_file( Load, Repo, Opts ),
    Clean = ( once(retract(lib_tables:lib_context(Repo,Root))) ),
    debug( lib, 'lib/4: ~w', setup_call_cleanup(Setup, Goal, Clean) ),
    setup_call_cleanup(Setup, Goal, Clean),
    findall( _, 
                    (   predicate_property(Repo:Ph,exported),
                        functor(Ph,Pn,Pa),
                        Cxt:import(Repo:Pn/Pa)
                    )
                    ,
                        _ ),
    memberchk( index(IdxB), Opts ),
    lib_attach_indices( IdxB, Root, Repo, Cxt ),
    memberchk( homonym(LocB), Opts ),
    lib_attach_filenames( LocB, Root, Repo, Cxt ).

lib_repo( Repo, Type, Root, Load, Cxt, Args ) :-
    Mess = 'Located in memory repo:~w of type: ~w, loading in: ~w, with root: ~w',
    debug( lib, Mess, [Repo,Type,Repo,Root] ),
    lib_defaults( Type, Defs ),
    append( Args, Defs, Opts ),
    lib( Repo, Root, Load, Cxt, Opts ),
    findall( _, (predicate_property(Repo:Phead,exported),functor(Phead,Pn,Pa),Cxt:import(Repo:Pn/Pa)), _ ).

lib_source( Repo, Opts ) :-
    prolog_load_context( directory, Base ), 
    directory_file_path( Root, prolog, Base ),
    % next N lines accommodate for private packs...
    directory_file_path( Root, src, Srot ),
    directory_file_path( Srot, packs, PrivP ),
    ( exists_directory(PrivP) -> assert( lib_tables:lib_packs_at(Repo,PrivP) ); true ),
    % end of N lines
    asserta( lib_tables:lib_context(Repo,Root) ),
    ( memberchk(index(Idx),Opts) -> true; Idx = false ),
    lib_source_index( Idx, Root, Repo ),
    ( memberchk(homonyms(Hmns),Opts) -> true; Hmns = false ),
    lib_source_homonyms( Hmns, Repo ).

lib_source_index( true, Root, Repo ) :-
    lib_src_sub_dir( Sub ),
    directory_file_path( Root, Sub, AbsSrc ),
    directory_file_path( AbsSrc, 'LibIndex.pl', LibIndex ),
    exists_file( LibIndex ),
    !,
    lib_load_index_file( LibIndex, Repo ).
lib_source_index( false, _Root, _Repo ).

lib_source_homonyms( true, Repo ) :-
    % :- dynamic( lib_tables:lib_loaded_homonyms/2 ).  % 
    lib_homonyms( Repo ),
    !.
lib_source_homonyms( false, _Repo ).

lib_source_end( Repo, _Opts ) :-
    retractall( lib_tables:lib_context(Repo,_Root1) ),
    retractall( lib_tables:lib_packs_at(Repo,_) ).
