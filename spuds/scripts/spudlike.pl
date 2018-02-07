:-  module( spudlike, [spudlike/0] ).

:- debug(spudlike).

/** <module> spudlike.

See doc for spudlike/0.

*/

/** spudlike.

    (Re-)Start a server at 3003 serving all currently installed packs.<br>
    Only tested on linux.

==
% pupsh spudlike
% was: /home/na11/bin/swipl_man_doc
==
@author nicos angelopoulos
@version  0.2 2018/01/26
@version  0.3 2018/02/07,  remove dependency to by_unix

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
    user:file_search_path( pack, Pack ), 
    AbsOpts = [file_type(directory),file_errors(fail)],
    absolute_file_name( Pack, Packed, AbsOpts ),
    !,
    debug( spudlike, 'Packs directory: ~p', Packed ),
    directory_files( Packed, AllSubs ),
    once( select('.',AllSubs,NodSubs) ),
    once( select('..',NodSubs,NtdSubs) ),
    % os_dir_dirs( Packed, Packs ),
    maplist( spudlike_load(Packed), NtdSubs ),
    write( 'http://localhost:3003/pldoc' ), nl.

spudlike_load( _, 'Downloads' ) :-
    !.
spudlike_load( Root, Pack ) :-
    directory_file_path( Root, Pack, Path ),
    exists_directory( Path ),
    !,
    debug( spudlike, '...loading: ~w', Pack ),
    ( catch(lib(Pack),_,fail) -> 
        true
        ;
        debug( spudlike, '...FAILED to load it', true )
    ).
spudlike_load( _Root, _Pack ).  % skipping litter files

spudlike_kill :-
    current_prolog_flag( pid, ThisPid ),
    debug( spudlike, 'This process id: ~d', ThisPid ), nl,
    % LnsPrv @@ psa('spudlike'),
    psa_lines( spudlike, LnsPrv ),
    exclude( atom_sub('grep cline'), LnsPrv, LnsCline ),
    include( atom_sub('swipl -x'), LnsCline, LnsSwi ),
    include( atom_sub('upsh'), LnsSwi, LnsUpsh ),
    include( atom_sub('swipl'), LnsUpsh, Lns ),
    member( Ln, Lns ),
    once( (atomic_list_concat([_|T],' ',Ln),member(Pid,T),Pid \== '',atom_number(Pid,PidNum) ) ),
    PidNum =\= ThisPid,
    debug( spudlike, 'Killing process id: ~d', PidNum ), nl,
    !,
    debug( spudlike, 'Killing old spudlike process: ~w', Pid ), nl,
    % @ kill( -9, Pid ).
    process_create( path(kill), ['-9',Pid], [] ).
spudlike_kill :-
    debug( spudlike, 'No running spudlike found.', true ).

psa_lines( spudlike, Lines) :-
        setup_call_cleanup(
            process_create(path(ps), [ '--columns',300, '-Af' ],
                           [ stdout(pipe(Out))
                           ]),
            read_lines(Out, Lines),
            close(Out)).

read_lines(Out, Lines) :-
        read_line_to_codes(Out, Line1),
        read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
        atom_codes(Line, Codes),
        read_line_to_codes(Out, Line2),
        read_lines(Line2, Out, Lines).

atom_sub( Sub, Atom ) :-
    sub_atom( Atom, _, _, _, Sub ),
    !.
