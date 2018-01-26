:- ensure_loaded( library(lib) ).
:- lib(by_unix).
:- lib(debug_call).
:- lib(stoics_lib:atom_sub/2).

:- debug(spudlike).

/** spudlike.

    (Re-)Start a server at 3003 serving all currently installed packs.<br>
    Only tested on linux.

==
% pupsh spudlike
% was: /home/na11/bin/swipl_man_doc
==
@author nicos angelopoulos
@version  0.2 2018/01/26

*/

spudlike :-
    spudlike_restart,
    www_open_url('http://localhost:3003/pldoc'),
    spudlike_busy.

spudlike_busy :-
    sleep( 10000 ),
    !,
    spudlike_busy.

spudlike_restart :-
    spudlike_kill,
    doc_server( 3003 ),
    write( 'http://localhost:3003/pldoc' ), nl,  % fixme: do in same fashion as the server, highjack call after
    use_module( library(lib) ),
    lib(os_lib),
    file_search_path( pack, Pack ), 
    AbsOpts = [file_type(directory),file_errors(fail)],
    absolute_file_name( Pack, Packed, AbsOpts ),
    !,
    debug( spudlike, 'Packs directory: ~p', Packed ),
    os_dir_dirs( Packed, Packs ),
    maplist( spudlike_load, Packs ),
    write( 'http://localhost:3003/pldoc' ), nl.

spudlike_load( 'Downloads' ) :-
    !.
spudlike_load( Pack ) :-
    debug( spudlike, '...loading: ~w', Pack ),
    ( catch(lib(Pack),_,fail) -> 
        true
        ;
        debug( spudlike, '...FAILED to load it', true )
    ).

spudlike_kill :-
    % fixme: from desktop invocation the 
    current_prolog_flag( pid, ThisPid ),
    write( this_pid(ThisPid) ), nl,
    % LnsPrv @@ psa('cline/spudlike.pl'),
    LnsPrv @@ psa('spudlike'),
    exclude( atom_sub('grep cline'), LnsPrv, LnsCline ),
    include( atom_sub('swipl -x'), LnsCline, LnsSwi ),
    include( atom_sub('upsh'), LnsSwi, LnsUpsh ),
    % exclude( atom_sub('spudlike_open'), LnsCline, LnsOpen ),
    include( atom_sub('swipl'), LnsUpsh, Lns ),
    member( Ln, Lns ),
    % Lns = [Ln|_],
    % atom_codes( Tab, [0'\t] ),
    once( (atomic_list_concat([_|T],' ',Ln),member(Pid,T),Pid \== '',atom_number(Pid,PidNum) ) ),
    PidNum =\= ThisPid,
    !,
    % debug_call( spudlike, ns_sel, Ln/Lns ),
    debug( spudlike, 'Killing old spulike process: ~w', Pid ), nl,
    % atom_number( Pid, PidNum ),
    % @ kill( -9, PidNum ).
    @ kill( -9, Pid ).
spudlike_kill :-
    debug( spudlike, 'No running spudlike found.', true ).
