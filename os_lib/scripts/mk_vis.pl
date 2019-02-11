
:- use_module(library(lib)).
:- lib(os_lib).
:- lib(by_unix).

:- debug(by_unix).

/**  mk_vis( +Dir ).

Make every file within Dir readable by group and others and ever directory is made readable and executable
by group and others.

Only works on *nix. Dir can be a singleton list (this plays better with pack(upsh)).

==
?- working_directory( pack(os_lib), _ ).
?- mkvis( '.' ).

% private test:
?- working_directory( '/home/nicos/web/sware/packs/bio_db_repo/', Old ).
?- mkvis( 'data-19.02.11' ).
==

@author nicos angelopoulos
@version  0.1 2019/2/11
@tbd  os/2 that incorporates os_file and os_dir.

*/
mk_vis( Dir ) :-
    working_directory( Old, Dir ),
    mk_vis_files,
    mk_vis_dirs,
    working_directory( _, Old ),
    @ chmod( go+rx, Dir ).

mk_vis_files :-
    os_file( File, sub(true) ),
    @ chmod( go+r, File ),
    fail.
mk_vis_files.

mk_vis_dirs :-
    os_dir( File, sub(true) ),
    @ chmod( go+rx, File ),
    fail.
mk_vis_dirs.
