
:- use_module(library(debug)).  % /3.
:- use_module(library(http/http_open)). % fixme: doit dynamically
:- use_module(library(listing)).       % portray_clause/2.

:- lib(get_datetime/1).
:- lib(suggests(options)).

url_file_defaults( [overwrite(error),dnt(false)] ).

%% url_file( +Url, ?File ).
%% url_file( +Url, ?File, +Opts ).
% 
% Get the remote file pointed to by Url to local File. <br>
% When File is an unbound variable, place the download into downloads(Base), if downloads is a known file alias,<br>
% or as Base in local directory, and return the used file in File. Base is taken as the file_base_name/2 of Url.
% 
% The predicate's progress can be be looked into, by ?- debug(url_file).
%
% The main download code is a copy-paste from SWI's library(prolog_pack) file.
% 
% Opts 
% * overwrite(Ow=error)
%   default throws an error if file exists, fail or false for failure and anything else for business as usual (overwrite local)
% * dnt(Dnt=false)
%   if true, create a File.dnt with the start and end datime/6 stamps.
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
%
url_file( Url, LocP ) :-
    url_file( Url, LocP, [] ).

url_file( Url, LocP, Opts ) :-
    var(LocP),
	user:file_search_path( downloads, Downloads ),
	!,
	make_directory_path( Downloads ),
	file_base_name( Url, RemB ),
	directory_file_path( Downloads, RemB, LocP ),
	url_file( Url, LocP, Opts ).
url_file( Url, RemB, Opts ) :-
    var(RemB),
    !,
	file_base_name( Url, RemB ),
	url_file( Url, RemB, Opts ).
url_file( Url, Local, Args ) :-
	options_append( url_file, Args, Opts ),
	options( overwrite(Ow), Opts ),
	url_file_ow( Ow, Url, Local, Opts ).

url_file_ow( false, Url, Local, _Opts ) :- 
	exists_file( Local ),
	!,
	debug( url_file, 'Local file exists: ~p, not downloading it again from: ~p.', [Local,Url] ),
    fail.
url_file_ow( fail, Url, Local, _Opts ) :- 
	exists_file( Local ),
	!,
	debug( url_file, 'Local file exists: ~p, not downloading it again from: ~p.', [Local,Url] ),
    fail.
url_file_ow( error, Url, Local, _Opts ) :-
	exists_file( Local ),
	!,
	throw( refusing_to_download_url_to_existing_file(Local,Url) ).
url_file_ow( _, Url, Local, Opts ) :- % fixme, add error value for ow() ?
	debug( url_file, 'Downloading URL: ~p, onto file: ~p', [Url,Local] ),
	get_datetime( StartDt ),
	setup_call_cleanup(
	    http_open(Url, In,
		      [ % cert_verify_hook(ssl_verify) % checkme:
		      ]),
	    setup_call_cleanup(
	    open(Local, write, Out, [type(binary)]),
	    copy_stream_data(In, Out),
	    close(Out)),
	    close(In)
	),
	get_datetime( EndDt ),
	options( dnt(Dnt), Opts ),
	url_file_dnt( Dnt, Local, StartDt, EndDt ).

url_file_dnt( true, Local, StartDt, EndDt ) :-
	file_name_extension( Local, dnt, DntF ),
	% os_ext( dnt, Local, DntF ),
	open( DntF, write, Out ),
	portray_clause( Out, StartDt ),
	portray_clause( Out, EndDt ),
	close( Out ).
url_file_dnt( false, _Local, _StartDt, _EndDt ).
