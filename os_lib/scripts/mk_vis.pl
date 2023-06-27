
:- use_module(library(apply)).
:- use_module(library(lib)).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).

:- debuc(by_unix).

mk_vis_defaults( who(go) ).

/**  mk_vis( +DirsAndOpts ).

Make every file within Dirs readable by group and others and ever sub-directory is made readable and executable
by Who (see Opts). All atomic entries in DirsAndOpts are taken to be directories to make (recursively) visible.
If no Dirs are found Dirs = ['.'].

Opts
 * who(Who=go)
 else set to g for only enhancing group privilages

Only works on *nix. Dir can be a singleton list (this plays better with pack(upsh)).

==
?- working_directory( pack(os_lib), _ ).
?- mkvis( '.' ).
?- mkvis( [] ).

% private test:
?- working_directory( '/home/nicos/web/sware/packs/bio_db_repo/', Old ).
?- mkvis( ['data-19.02.11',who(g)] ).
==

@author nicos angelopoulos
@version  0.1 2019/2/11
@version  0.1 2019/3/04,  added Opt who(Who)
@tbd  os/2 that incorporates os_file and os_dir.

*/
mk_vis( Args ) :-
    options_append( mk_vis, Args, Opts, atoms(DirsPrv) ),
    ( DirsPrv = [] -> Dirs = ['.']; Dirs = DirsPrv ),
    options( who(Who), Opts ),
    atomic_list_concat( [Who,r], '+', Fho ),
    atomic_list_concat( [Who,rx], '+', Dho ),
    maplist( mk_vis_dir(Fho,Dho), Dirs ).

mk_vis_dir( Fho, Dho, Dir ) :-
    working_directory( Old, Dir ),
    mk_vis_files( Fho ),
    mk_vis_dirs( Dho ),
    working_directory( _, Old ),
    % @ chmod( go+rx, Dir ).
    @ chmod( Dho, Dir ).

mk_vis_files( Fho ) :-
    os_file( File, sub(true) ),
    @ chmod( Fho, File ),
    fail.
mk_vis_files( _ ).

mk_vis_dirs( Dho ) :-
    os_dir( File, sub(true) ),
    % @ chmod( go+rx, File ),
    @ chmod( Dho, File ),
    fail.
mk_vis_dirs( _ ).
