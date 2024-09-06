
:- use_module(library(debug)).          % /3.
:- use_module(library(process)).        % process_create/3.
:- use_module(library(readutil)).       % read_line_to_codes/2.
:- use_module(library(http/http_open)). % fixme: doit dynamically
:- use_module(library(listing)).        % portray_clause/2.
:- use_module(library(socket)).         % gethostname/1.

:- lib(at_con/3).
:- lib(get_datetime/1).
:- lib(suggests(options)).

url_file_extract( gz, gunzip, ['-k'] ).

url_file_defaults( Defs ) :-
                         Defs  =  [ 
                                     dn_dir(Downloads),
                                     dnt(false),
                                     hash(Hasher),
                                     extract(Xtc),
                                     iface(prolog),
                                     insecure(false),
                                     overwrite(error)
                                  ],
        ( user:file_search_path(downloads,Downloads) -> true; Downloads = '.' ),
        DefHasher = sha256sum,
        ( catch(process_create(path(which),[DefHasher],[stdout(null)]),_,fail) -> DefHasher = Hasher ; Hasher = false ),
        ( catch(process_create(path(which),[gunzip],[stdout(null)]),_,fail) -> Xtc = true ; Xtc = false ).

%% url_file( +Url, ?File ).
%% url_file( +Url, ?File, +Opts ).
% 
% Get the remote file pointed to by Url to local File.
% 
% When File is an unbound variable, place the download into downloads(Base), if downloads is a known file alias,
% or as Base in local directory, and return the used file in File. Base is taken as the file_base_name/2 of Url.
% 
% The predicate's progress can be be looked into, by ?- debug(url_file).
%
% The main download code is a copy-paste from SWI's library(prolog_pack) file.
% 
% Opts 
% * dn_dir(DnDir)
%   downloads direcotry. Def is =|user:file_search_path(downloads,DnDir)|= if it exists and _./_ otherwise.
% * dnt(Dnt=false)
%   if _pl_ or _true_ creates a File.dnt with the start and end datime/6 term stamps (v0.4: wrapped in start(), end()).<br>
%   if _csv_ creates File.csv with the same info as dnt but in csv format (2 columns)
%   From v0.4 also records the Url, the download host and hash value for the download (see hash())
% * hash(Hash=sha256sum)
%   executable for creating a hash which is registered in .dnt or .csv file (dnt()). Use _false_ for no hash, and no relavant entry.<br>
%   Default is _sha256sum_ if the executable is in PATH other wise false.
% * extract(Xtc)
%   whether to uncomompress/extract file if download has recognisable compressed extension (_true_ if gunzip in PATH; _false_ otherwise)
% * iface(Iface=prolog)
%   or _wget_
% * insecure(Insec=false)
%   where to acccept non SSL authenticated connections (as in pack_install/2, 24.04.05)
% * overwrite(Ow=error)
%   default throws an error if file exists, _fail_ or _false_ for failure and anything else for business as usual (overwrite local)
%
%==
% ?- file_search_path( downloads, Dnloads ).
% Dnloads = '/usr/local/users/nicos/local/dnloads'.
%
% ?- url_file( 'http://stoics.org.uk/~nicos/index.html', File ).
% File = '/usr/local/users/na11/local/dnloads/index.html'.
%
% ?- debug( url_file ).
% ?- url_file('ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz').
% Downloading URL: 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz', onto file: '/usr/local/users/nicos/local/dnloads/gene2ensembl.gz'
% ?- ls( '/usr/local/users/nicos/local/dnloads/' ).
% ...
% gene2ensembl.gz
% ...
%
% ?- retractall(  user:file_search_path( downloads, Dn ) ).
% true.
% ?- url_file( 'http://stoics.org.uk/~nicos/index.html', File ).
% File = index.html.
% ?- ls.
% .... index.html ....
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/07/23
% @version  0.2 2015/11/24  added option overwrite/1
% @version  0.3 2018/03/13, removed url_file/1 but url_file/2 allows -File, moved to pack(stoics_lib)
% @version  0.4 2024/09/06, added dnt(csv) & true/pl; hash(); extract(); dn_dir()
%
url_file( Url, LocP ) :-
     url_file( Url, LocP, [] ).
url_file( Url, RemB, Opts ) :-
     var(RemB),
     !,
     options( dn_dir(Dnloads), Opts ),
     file_base_name( Url, RemB ),
     ( Dnloads = '.' -> 
          RemB = LocP
          ;
          make_directory_path( Dnloads ),
          directory_file_path( Dnloads, RemB, LocP )
     ),
     url_file( Url, LocP, Opts ).
url_file( Url, Local, Args ) :-
     options_append( url_file, Args, Opts ),
     options( overwrite(Ow), Opts ),
     options( iface(Iface), Opts ),
     url_file_ow( Ow, Iface, Url, Local, Opts ).

url_file_ow( false, _Iface, Url, Local, _Opts ) :- 
     exists_file( Local ),
     !,
     debug( url_file, 'Local file exists: ~p, not downloading it again from: ~p.', [Local,Url] ),
    fail.
url_file_ow( fail, _Iface, Url, Local, _Opts ) :- 
     exists_file( Local ),
     !,
     debug( url_file, 'Local file exists: ~p, not downloading it again from: ~p.', [Local,Url] ),
    fail.
url_file_ow( error, _Iface, Url, Local, _Opts ) :-
     exists_file( Local ),
     !,
     throw( refusing_to_download_url_to_existing_file(Local,Url) ).
url_file_ow( _, Iface, Url, Local, Opts ) :- % fixme, add error value for ow() ?
     debug( url_file, 'Downloading URL: ~p, onto file: ~p, with interface: ~w', [Url,Local,Iface] ),
     get_datetime( StartDt ),
     options( insecure(Insec), Opts ),
     url_file_dnload( Iface, Insec, Url, Local ),
     get_datetime( EndDt ),
     options( dnt(Dnt), Opts ),
     ( Dnt == csv -> use_module(library(csv)); true ),
     url_file_dnt( Dnt, Local, StartDt, EndDt, Url, Opts ).

url_file_dnload( prolog, Insec, Url, Local ) :-
     ( Insec == true -> OpenOpts = [cert_verify_hook(ssl_verify_null)] ; OpenOpts = [] ),
     setup_call_cleanup(
         http_open(Url, In, OpenOpts),
         setup_call_cleanup(
         open(Local, write, Out, [type(binary)]),
         copy_stream_data(In, Out),
         close(Out)),
         close(In)
     ).
url_file_dnload( wget, Insec, Url, Local ) :-
     ( Insec == true -> 
                    at_con(['wget --no-check-certificate -O',Local,Url], ' ', Wget) 
                    ; 
                    at_con(['wget -O',Local,Url], ' ', Wget) 
     ),
     shell( Wget ).

url_file_dnt( false, _Local, _StartDt, _EndDt, _Url, _Opts ) :- !.
url_file_dnt( RecAsPrv, Local, StartDt, EndDt, Url, Opts ) :-
     ( RecAsPrv == csv -> 
          RecAs = csv,
          file_name_extension( Local, dnv, DntF )
          ;
          RecAs = pl,
          file_name_extension( Local, dnt, DntF )
     ),
     gethostname( Hname ),
     % os_ext( dnt, Local, DntF ),
     open( DntF, write, Out ),
     url_file_dnt_rec( RecAs, Out, start(StartDt) ),
     url_file_dnt_rec( RecAs, Out, end(EndDt) ),
     url_file_dnt_rec( RecAs, Out, url(Url) ),
     url_file_dnt_rec( RecAs, Out, dnl_host(Hname) ),
     options( hash(Hopt), Opts ),
     url_file_dnt_hash( Hopt, Local, RecAs, Out ),
     options( extract(Xtc), Opts ),
     url_file_dnt_extract( Xtc, Hopt, Local, RecAs, Out ),
     close( Out ).

url_file_dnt_extract( true, Hopt, Local, RecAs, Out ) :-
     file_name_extension( Base, Ext, Local ),
     ( url_file_extract(Ext,Exec,Args) -> 
                    append( Args, [Local], Alls ),
                    process_create(path(Exec),Alls,[]),
                    url_file_dnt_hash( Hopt, Base, RecAs, Out )
                    ;
                    true
     ).
url_file_dnt_extract( false, _Hopt, _Local, _RecAs, _Out ).

url_file_dnt_rec( csv, Out, Term ) :-
     Term =.. [Key|Args],
     ( (Key==start;Key==end) ->
          Args = [datetime(Y,M,D,Hr,Mn,Sc)],
          atomic_list_concat( [Y,',',M,',',D,'@',Hr,':',Mn,':',Sc], '', Narg ),
          Nargs = [Key,Narg]
          ;
          Nargs = [Key|Args]
     ),
     length( Nargs, Nlen ),
     Rem is 4 - Nlen,
     findall( '', between(1,Rem,_), Pads ),
     append( Nargs, Pads, Alls ),
     Row =.. [row|Alls],
     csv_write_stream( Out, [Row], [] ).
url_file_dnt_rec( pl, Out, Term ) :-
     portray_clause( Out, Term ).

url_file_dnt_hash( false, _File, _RecAs, _Out ) :- !.
url_file_dnt_hash( Hasher, File, RecAs, Out ) :-
     setup_call_cleanup(
            process_create(path(Hasher), [File],
                           [ stdout(pipe(Piped))
                           ]),
               ( read_line_to_codes( Piped, Codes ),
                 atom_codes( Atom, Codes ) ),
            close(Piped)),
     atomic_list_concat( Parts, ' ', Atom ),
     once( append([Hash|_], [File], Parts) ),
     url_file_dnt_rec( RecAs, Out, hash(Hasher,File,Hash) ).
     
%   ssl_verify_null(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%   Copied from system library(prolog_pack.pl).
%  2024.04.05
%
:- public ssl_verify_null/5.
ssl_verify_null(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).

