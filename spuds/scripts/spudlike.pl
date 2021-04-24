:-  module( spudlike, [spudlike/0,spudlike/1] ).

:- use_module(library(lists)).          % append/3.
:- use_module(library(apply)).          % exclude/3, include/3.
:- use_module(library(debug)).          % /1,3.
:- use_module(library(process)).        % process_create/3.
:- use_module(library(readutil)).       % read_line_to_codes/2.
:- use_module(library(doc_http)).       % doc_server/2.
:- use_module(library(www_browser)).    % www_open_url/1.

:- debug(spudlike).

/** <module> spudlike.

See doc for spudlike/0.

*/

/** spudlike.
    spudlike(Opts).

    (Re-)Start a server at Port serving all currently installed packs.<br>
    Only tested on linux.

==
% upsh spudlike
==

Opts

  * allow(Allow=[])
  Ips allowed to connect to server

  * browser(Browser=true)
  whether to start a browser window authomatically. if not a boolean is taken to be the page to start

  * ignore(AbsIgnore=_)
  ignore file in this absolute location

  * kill(Kill=true)
  whether to kill existing server

  * port(Port=3003)
  port for server

  * scripts(Scripts=false)
  experimental: untested, whether to load to server, upsh locatable scripts

  * server(Server=localhost)
  domain name of the server to access

You can add something like the following to a launcher in your linux windows manager:
==
/usr/local/users/nicos/local/git/bin/swipl -f none 
    -g "doc_collect(true),[pack(spuds/scripts/spudlike)]" -t spudlike

/usr/local/users/nicos/local/git/bin/swipl -f none 
    -g "[pack(spuds/scripts/spudlike)]" -g spudlike

/usr/local/users/nicos/local/git/bin/swipl -f none 
    -g "doc_collect(true),[pack(spuds/scripts/spudlike)]" 
        -g "spudlike(browser('search?for=stoics_lib&in=all&match=summary'))"

==
which will also give you doc for any start-up loading packs (and spudlike/0,1). The "-f" is critical in that normal user profile is not loaded before
the server has been started (in which case it will not be in the doc server).

@author nicos angelopoulos
@version  0.2 2018/01/26
@version  0.3 2018/02/07,  removed dependency to by_unix
@version  0.4 2019/03/21,  generalise for home network use. added options and CLI example
@version  0.5 2020/03/19,  steadfast kill code; doc enhancements; work in tmp dir courtesy tmp_file/2; browser to page
@version  0.6 2020/08/20,  first take on running on macs (no killing yet)
*/

spudlike :-
    spudlike( [] ).
spudlike( ArgS ) :-
    ( is_list(ArgS) -> Args=ArgS; Args=[ArgS] ),
    tmp_file( spudlike, Tmp ),
    make_directory( Tmp ),
    working_directory( _, Tmp ),
    debug( spudlike, 'Working dir: ~w', [Tmp] ),
    ( getenv('HOST',Host) -> 
        atomic_list_concat( ['spudlike_', Host, '.pl'], HostPrefsB )
        ;
        % Host = localhost,
        HostPrefsB = 'spudlike.pl'
    ),
    getenv('HOME',Home),
    directory_file_path(Home,'.pl', HostPrefsD),
    directory_file_path(HostPrefsD,HostPrefsB,AbsHostPrefsF),
    ( exists_file(AbsHostPrefsF) ->
        debug( spudlike, 'Preferences from: ~w', [AbsHostPrefsF] ),
        spudlike_read_file( AbsHostPrefsF, UserOpts )
        ;
        %  atom_concat( '~/', HostPrefsB, RepPrefsF ),
        debug( spudlike, 'No preferences file found (~p).', [AbsHostPrefsF] ),
            % fixme, also for .pl/spudlike.pl here
        UserOpts = []
    ),
    append( Args, UserOpts, Opts ),
    debug( spudlike, 'Opts: ~w', [Opts] ),
    options_val( port(Port), Opts, 3003 ),
    options_val( server(Server), Opts, localhost ),
    options_val( kill(Kill), Opts, true ),
    % ( memberchk(port(Port),Opts) -> true; Port = 3003 ),
    % ( memberchk(server(Server),Opts) -> true; Server = localhost ),
    % ( memberchk(kill(Kill),Opts) -> true; Kill = true ),
    findall( allow(Allow), member(allow(Allow),Opts), AllowsPrv ),
    ( AllowsPrv = [] -> 
        ( Server == localhost ->
            Allows = [] % doc_server/2 defaults are fine
            ;
            % process_output( hostname, ['-I'], Atom ),
            process_output( hostname, [], Atom ),
            atom_concat( IP, ' \n', Atom ),
            Allows = [allow(IP)]
        )
        ;
        AllowsPrv = Allows
    ),
    debug( spudslike, 'Server: ~w, Port: ~w', [Server,Port] ),
    options_val( scripts(Scripts), Opts, true ),
    findall( Ign, member(ignore(Ign),Opts), Ignore ),
    spudlike( Server, Port, Allows, Kill, Scripts, Ignore ),
    ( memberchk(browser(Browser),Opts) -> true; Browser = true ),
    debug( spudslike, 'Browser: ~w', [Browser] ),
    ( Browser == false ->
        true
        ;
        ( Browser == true ->
            atomic_list_concat( ['http://',Server,':',Port,'/pldoc'], '', Url )
            ;
            atomic_list_concat( ['http://',Server,':',Port,'/pldoc/',Browser], '', Url )
        ),
        debug( spudslike, 'URL: ~w', [Url] ),
        www_open_url(Url)
    ),
    spudlike_busy.

spudlike_busy :-
    sleep( 10000 ),
    !,
    spudlike_busy.

spudlike( localhost, Port, Allows, Kill, Scripts, Ignore ) :-
    spudlike_kill( Kill, localhost ),
    doc_server( Port, Allows ),
    debug( spudlike, 'doc_server(~w,~w)', [Port,Allows] ),
    use_module( library(lib) ),
    user:file_search_path( pack, Pack ), 
    AbsOpts = [file_type(directory),file_errors(fail)],
    absolute_file_name( Pack, Packed, AbsOpts ),
    !,
    debug( spudlike, 'Packs directory: ~p', Packed ),
    directory_files( Packed, WithDotsSubs ),
    once( select('.',WithDotsSubs,NodSubs) ),
    once( select('..',NodSubs,Subs) ),
    % os_dir_dirs( Packed, Packs ),
    maplist( spudlike_load(Packed), Subs ),
    spudlike_scripts( Scripts, Packed, Subs, Ignore ),
    atomic_list_concat( ['http://localhost:',Port,'/pldoc'], '', Url ),
    debug( spudlike, '~w', [Url] ).
spudlike( Remote, _Port, _Allows, _Kill, _Scripts, _Ignore ) :-
    % fixme: pass Opts ?
    getenv( 'USER', User ),
    atomic_list_concat( [User,Remote], '@', From ),
    process_create( path(ssh), ['-Y',From,pupsh,spudlike], [] ).

spudlike_scripts( true, _Packed, _PackSubs, Ignore ) :- 
    !,
    % fixme(here).
    % do the bin first
    % use tmp module ?
    % user:file_search_path( home, Home ),
    % ( expand_file_name( '$HOME', [Home|_] ) -> true; Home = '/home/nicos' ),
    expand_file_name( '$HOME', [Home|_] ),
    directory_file_path( Home, bin, Bin ),
    directory_file_path( Bin, cline_upsh, Cline ),
    ( exists_directory(Cline) ->
          directory_files( Cline, ClineSubs ),
          /* 
          
          ignores([/home/nicos/bin/cline_upsh/publish_pack.pl])
          plp(/home/nicos/bin/cline_upsh/publish_pack.pl)
                         
          */
          findall( _, ( member(PlF,ClineSubs), atom_concat(_,pl,PlF), write(doing(PlF)),nl,
                         directory_file_path(Cline, PlF, PlP),
                         \+ memberchk(PlP, Ignore),
                         consult(tmp:PlP)
                         % consider abolishing all tmp:_ ?
                         ),
                              _ )
    % then do the Subs (again tmp:module)
    % fixme(Subs).
          ;
          true
    ),
    true.
spudlike_scripts(_,_,_,_).

spudlike_load( _, 'Downloads' ) :-
    !.
spudlike_load( Root, Pack ) :-
    directory_file_path( Root, Pack, Path ),
    exists_directory( Path ),
    !,
    debug( spudlike, '...loading: ~w', Pack ),
    spudlike_loads( Pack, LoadThis ),
    ( catch(lib(LoadThis),_,fail) -> 
        true
        ;
        debug( spudlike, '...FAILED to load it', [] )
    ).
spudlike_load( _Root, _Pack ).  % skipping litter files

spudlike_loads( Pack, LoadThis ) :-
    spudlike_loads_specific( Pack, LoadThis ),
    !.
spudlike_loads( Pack, Pack ).

spudlike_kill( true, localhost ) :-   % only attempt when running locally
    current_prolog_flag( unix, true ),
    current_prolog_flag( pid, ThisPid ),
    debug( spudlike, 'This process id: ~d', ThisPid ), nl,
    psa_lines( spudlike, LnsPrv ),
    exclude( atom_sub('grep cline'), LnsPrv, LnsCline ),
    include( atom_sub('-g spudlike'), LnsCline, LnsG ),
    ( LnsG = [] -> 
        include( atom_sub('-t spudlike'), LnsCline, Lns )
        ;
        Lns = LnsG
    ),

    % ( (include( atom_sub('swipl'),LnsCline,LnsSwi),write(here(LnsSwi)), nl,LnsSwi\==[]) -> true; LnsSwi = LnsCline ),
    % debug( spudlike, 'Swi lines: ~w', [LnsSwi] ),
    % upsh: (but currently SWI throws error anyway
    % ( (include( atom_sub('swipl -f'),LnsCline,LnsSwi),LnsSwi\==[]) -> true; LnsSwi = LnsCline ),
    % ( (include(atom_sub('upsh'),LnsSwi,LnsUpsh),LnsUpsh \==[]) -> true; LnsUpsh = LnsSwi ),
    % include( atom_sub('swipl'), LnsUpsh, Lns ),
    % % findall( Aln, (member(Aln,LnsSwi),atom_concat(_,spudlike,Aln)), Lns ),
    % include( atom_sub('spudlike'),LnsCline,LnsSwi),write(here(LnsSwi)), nl,LnsSwi\==[]) -> true; LnsSwi = LnsCline ),

    debug( spudlike, 'Identified lines: ~w', [Lns] ),
    member( Ln, Lns ),
    once( (atomic_list_concat([_|T],' ',Ln),member(Pid,T),Pid \== '',atom_number(Pid,PidNum) ) ),
    PidNum =\= ThisPid,
    debug( spudlike, 'Killing process id: ~d', PidNum ), nl,
    !,
    debug( spudlike, 'Killing old spudlike process: ~w', Pid ), nl,
    catch( process_create( path(kill), ['-9',Pid], [] ), _, true ),
    sleep(3).
spudlike_kill( true, _Server ) :-
    current_prolog_flag( unix, true ),
    !,
    debug( spudlike, 'No running spudlike found.', [] ).
spudlike_kill( _, _Server ). 
    % SWI will throw an error if an old instance is running... so let it succeed here

psa_lines( spudlike, Lines) :-
    current_prolog_flag( apple, true ),
    !,
    setup_call_cleanup(
            process_create(path(ps), ['-Af' ],
                           [ stdout(pipe(Out))
                           ]),
            read_lines(Out, Lines),
            close(Out)).
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

options_val( Compound, Opts, Def ) :-
    ( memberchk(Compound,Opts) -> true; arg(1,Compound,Def) ),
    !.

spudlike_loads_specific( terminus_store_prolog, terminus_store ).
