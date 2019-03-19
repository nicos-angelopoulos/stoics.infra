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
@version  0.3 2018/02/07,  removed dependency to by_unix
@version  0.4 2019/03/18,  generalise for home network use. added options.


*/

spudlike :-
    spudlike( [] ).
spudlike( Args ) :-
    getenv( 'HOST', Host ),
    atomic_list_concat( ['.pl/spudlike_', Host, '.pl'], HostPrefs ),
    absolute_file_name( home(HostPrefs), AbsHostF ),
    debug( spudlike, 'AbsHostF: ~w', [AbsHostF] ),
    ( exists_file(AbsHostF) ->
        spudlike_read_file( AbsHostF, UserOpts )
        ;
        % fixme, also for .pl/spudlike.pl here
        UserOpts = []
    ),
    append( Args, UserOpts, Opts ),
    debug( spudlike, 'Opts: ~w', [Opts] ),
    ( memberchk(port(Port),Opts) -> true; Port = 3003 ),
    ( memberchk(server(Server),Opts) -> true; Server = localhost ),
    ( memberchk(kill(Kill),Opts) -> true; Kill = true ),
    findall( allow(Allow), member(allow(Allow),Opts), AllowsPrv ),
    ( AllowsPrv = [] -> 
        ( Server == localhost ->
            Allows = [] % doc_server/2 defaults are fine
            ;
            process_output( hostname, '-I', Atom ),
            atom_concat( IP, ' \n', Atom ),
            Allows = [allow(IP)]
        )
        ;
        AllowsPrv = Allows
    ),
    spudlike( Port, Server, Allows, Kill ),
    ( memberchk(browser(Browser),Opts) -> true; Browser = true ),
    ( Browser == false ->
        true
        ;
        atomic_list_concat( ['http://',Host,':',Port,'/pldoc'], '', Url ),
        % www_open_url('http://localhost:8080/pldoc')
        www_open_url(Url)
    ),
    spudlike_busy.

spudlike_busy :-
    sleep( 10000 ),
    !,
    spudlike_busy.

spudlike( Port, Server, Allows, Kill ) :-
    spudlike_kill( Kill, Server ),
    doc_server( Port, Allows ),
    debug( spudlike, 'doc_server(~w,~w)', [Port,Allows] ),
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
    write( 'http://localhost:8080/pldoc' ), nl.

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

spudlike_kill( true, localhost ) :-   % only attempt when running locally
    current_prolog_flag( unix, true ),
    current_prolog_flag( pid, ThisPid ),
    debug( spudlike, 'This process id: ~d', ThisPid ), nl,
    % LnsPrv @@ psa('spudlike'),
    psa_lines( spudlike, LnsPrv ),
    exclude( atom_sub('grep cline'), LnsPrv, LnsCline ),
    ( (include( atom_sub('swipl -x'),LnsCline,LnsSwi),LnsSwi\==[]) -> true; LnsSwi = LnsCline ),
    ( (include(atom_sub('upsh'),LnsSwi,LnsUpsh),LnsUpsh \==[]) -> true; LnsUpsh = LnsSwi ),
    include( atom_sub('swipl'), LnsUpsh, Lns ),
    member( Ln, Lns ),
    once( (atomic_list_concat([_|T],' ',Ln),member(Pid,T),Pid \== '',atom_number(Pid,PidNum) ) ),
    PidNum =\= ThisPid,
    debug( spudlike, 'Killing process id: ~d', PidNum ), nl,
    !,
    debug( spudlike, 'Killing old spudlike process: ~w', Pid ), nl,
    % @ kill( -9, Pid ).
    process_create( path(kill), ['-9',Pid], [] ),
    sleep(3).
spudlike_kill( true, _Server ) :-
    current_prolog_flag( unix, true ),
    !,
    debug( spudlike, 'No running spudlike found.', true ).
spudlike_kill( _, _Server ). 
    % SWI will throw an error if an old instance is running... so let it succeed here

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

spudlike_read_file( File, Terms ) :-
    open( File, read, In ),
    read( In, Term ),
    spudlike_read_stream( Term, In, Terms ),
    close( In ).

spudlike_read_stream( end_of_file, _In, Terms ) :-
    !,
    Terms = [].
spudlike_read_stream( InTerm, In, Terms ) :-
    Terms = [InTerm|Tail],
    read( In, Next ),
    spudlike_read_stream( Next, In, Tail ).

process_output( Exe, Args, Atom ) :-
   Opts = [stdout(pipe(Out))],
   Create = process_create(path(Exe),Args,Opts),
   setup_call_cleanup( Create, read_string(Out,_,Output), close(Out) ),
   atom_string( Atom, Output ).
