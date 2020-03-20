:- module( spuds, [
					spuds/1,
					spuds_pid/1,
					spuds_stop/0,
					spuds_stop_pid/1,
					spuds_start/0,
					spuds_restart/0,
					spuds_version/2,
					spuds_profile_file/1
				] ).
					
:- use_module( library(debug) ).

/** <module> persistent, user code inclusive, doc server for linux

This library is largely obsolete, however examples/spudlike.pl is actively maintained.


  This library creates, uses and manages a persistent documentation server on a single port. 
In addition to serving all Prolog library code as per doc_server/1, spuds/1 also serves
installed packs and all user code declared as such, on the same server.

spuds is available as a SWI-Prolog pack, and can be installed the usual way.
==
	pack_install( spuds ).
==

Since 0:1:0 spuds supports http_daemon/0. It also provides a swipl-spuds bash shell file 
(pack(spuds/server/'swipl-spuds')),
to be placed in the /etc/init.d/ directory. This is a direct copy from the SWI file.
It follows well after standard scripts for servers starting/stopping. 
And can be used when you want all documents to be available as an independent server.

Pack files from =|swi(pack/.../prolog/...)|= are loaded via 
=|use_module(File,[]])|=.User documentation files  are also
loaded via use_module if their first term is a module declaration, otherwise
their comments are loaded by
=|xref_source(Abs,[comments(store),silent(Silent)])|=
use_module/2 has the side-effect of loading the code on to the server.
If we were to load module files by xref/2, the downside would be that
as far as we understand SWI will not serve the module page correctly
(particularly missing the top comments that describe module files).

Currently to ask for help users need to call =|spuds(Topic)|= although ideally one
can use prolog:help_hook/1 in which case normal route via help/1 should be possible.

When looking for help pages, the library tries to locate a running spuds server.
If one is not found, it starts one. The behaviour of this server
can be controlled via a set of predefined terms/clauses in a user profile file.
spuds/1 also starts a new web browser window pointing to the spuds server 
on the requested topic.

The default profile location is =|$HOME/.pl/spuds_profile_<Hostname>.pl|=  or
=|$HOME/.pl/spuds_profile.pl|= . 
An alternative location can be provided via (user:)spuds_profile/1.

In you profile you can add:
	
	* blocked_sys_library( 'jpl.pl' )      if any of those are defined matcing system libraries, (whole dirs or 
	                                       source files)  the corresponding parts will be ignored. 
								    In this case so_sub/3 is used instead of doc_load_library/1.


	* doc_server_default( port, 4001 )           port for the spuds server

	* doc_server_default( start_opens_browser, false )   should spuds_start, start a browser ?

	* file_is_blocked_prolog_source( +AbsFile )  .pl file that shouldn't be served

	* file_is_prolog_source( +AbsFile )          non .pl file that should be served

	* prolog_source_directory( -Dir )            all Prolog files in Dir will be served

	* prolog_source_directory( -Dir, -Opts )     all Prolog files in Dir will be served 
	                                             if returned by os_sub(Dir,Opts).

	* prolog_source_file( -File )                serve this file

	* debug(spuds)                               be verbose (reporting in /tmp/spuds_debug.txt)

Docs for each =|prolog_source_file|= are loaded to the spuds server by means of: 
use_module( File, [] ) if the File in question is a module file or 
=|xref_source(File,[comments(store),silent(true)])|= otherwise.

Documentation for each prolog file within each =|prolog_source_directory|= is also 
loaded similarly. Note you need library(os_sub) for this to work.
This is available as a pack install as per usual via

==
	pack_install( os_sub ).
==

Within each source directory (Dir above) files are considered to be Prolog source 
if there is no barring success of the call file_is_blocked_prolog_source/1
and either file has extension 'pl' or the call file_is_prolog_source/1 succeeds.
These calls are performed with the absolute location of the file as argument.

The server only works under linux. Compatibility patches to other platforms are very welcome.
For MacOs there should be a trivial change on the ps flags, if that, that should
be sufficient. For other systems we need

	* ability to start background prolog script from within Prolog

	* ability to get pid from script name (can work around this one with pid files)

	* ability to kill a process by script name or pid.

To pick changes to the user code base up and even those on Swi (say after you installed
a new version) the server needs to be restarted. This can be done via spuds_restart/0.

One scenario for starting the server as a debian server, including at start time,
copy spuds/server/swipl-spuds into /etc/init.d/ then follow the instrunctions in the file.
Usually you want to make a clean place where from the server runs.
So for instance create /srv/www/html/spuds/ and copy into it spuds/server/spuds_daemon.pl (along with spuds_docs.pl from same place).
On you window manager you can then have a launcher that points to:
==
	$PREFIX/swipl -f none -g "use_module(library(www_browser)), www_open_url('http://localhost:4004/index.html'), sleep(1), halt(0)"
==

spuds stands for, Spuds Persistent User-code-inclusive Documentation Server


@author         Nicos Angelopoulos
@version        0:1:5
@license        Perl Artistic
@see            http://stoics.org.uk/~nicos/sware/spuds
@see            pack(spuds)
@see            pack(spuds/profiles)
@tbd            support for other oses (MacOs should be a matter of using the correct ps flags).
@tbd            add seconds delay option

*/

spuds_srv_basename( spudsd ).

:- use_module( library(http/http_client) ). 	% http_get/3.

%% spuds_version( -Version, -Date ).
%
%  Version and release Date (date(Y,M,D) term).
spuds_version( 1:1:0, date(2020,3,19) ).

%% spuds_pid( -Pid ).
% 
%  Process id of any running spud server(s). Non-deterministic.
%  There should ever only be one process maximum, if all is working properly.
%
spuds_pid( Pid ) :-
	spuds_pid( Pid, info ).

spuds_pid( Pid, _ ) :-
	spuds_srv_basename( Bname ),
	atomic_list_concat( ['spuds/server/',Bname,'.pl'], Search ),
	ps_swipl_rows_with( Search, SdocRows ),
	member( Row, SdocRows ),
	arg( 2, Row, Pid ),
	!.
spuds_pid( Pid, Empty ) :-
	var( Pid ),
	spuds_pid_empty( Empty ).

spuds_pid_empty( info ) :-
	print_message( informational, spuds(no_srv) ),
	fail.
% spuds_pid_empty( quiet ) :- fail
	

%% spuds_stop.
%
% Kill the spuds server. True iff a single server can be found.
% If there are mulitple servers funning use spuds_stop_pid/1 to force a quit.
%
spuds_stop :-
	spuds_stop_empty( info ).

spuds_stop_empty( OnEmpty ) :-
	findall( Pid, spuds_pid(Pid,quiet), Pids ),
	spuds_stop_singletton( Pids, OnEmpty ).

%% spuds_stop_pid( Pid ).
%
%  Kill the spuds server with process id, Pid. The predicate checks 
%  in the output of =|ps|= linux command to verify that the process
%  id matches to a spuds server.
%
spuds_stop_pid( Pid ) :-
	spuds_srv_basename( Bname ),
	atomic_list_concat( ['spuds/server/',Bname,'.pl'], Search ),
	ps_swipl_rows_with( Search, SdocRows ),
	% row_is_unique( SdocRows, Pattern, Row ),
	member( Row, SdocRows ),
	arg( 2, Row, Pid ),
	kill( Pid, '-HUP' ),
	!.
spuds_stop_pid( Pid ) :-
	print_message( error, error(spuds(no_spuds_pid(Pid))) ).

%% spuds_restart.
%  
%  Restart the spuds server. It does not complain if one cannot be found.
%  No browser tabs/windows are open.
%
spuds_restart :-
	spuds_stop_empty( quiet ),
	spuds_start_server.

%% spuds_start.
%  
%  Start the spuds server. It prints an error and aborts if one is already running.
%
spuds_start :-
	spuds_pid( Pid, quiet ),
	!,
	throw( error(spuds(srv_exists(Pid))) ).
spuds_start :-
	spuds_start_server,
	print_message( informational, spuds(spuds_splash) ).

%% spuds( Topic ).
%
%  Open a new web browser tab (www_open_url/1) on spuds server for query Topic.
%  If there is no spuds server running, start one. 
%  This is run in the background and will persist beyond the limits
%  of the current session. The spuds server can be managed with spuds_stop/0,
%  spuds_pid/1 and spuds_restart/0 from any Prolog session.
%
spuds( Topic ) :-
	Host = localhost,
	spuds_def( port, Port ), 
	ensure_server_is_running( Host, Port ), 
	help_at( Host, Port, Topic ).

/* ideally we want :

:- multifile( prolog:help_hook/1 ).

prolog:help_hook( Topic ) :-
	write( topic(Topic) ), nl,
	% http://localhost:46027/search?for=help&in=all&match=summary
	fail.
	
*/

% auxiliary predicates.

spuds_stop_singletton( [], OnEmpty ) :-
	spuds_stop_at_empty( OnEmpty ).
spuds_stop_singletton( [Pid], _ ) :-
	!,
	spuds_stop_pid( Pid ).
% spuds_stop_singletton( [] ) :-   % already get a message from spuds_id/1.
spuds_stop_singletton( [P,I|Ds], _ ) :-
	print_message( informational, spuds(too_many_srvs_to_stop([P,I|Ds])) ),
	fail.

spuds_stop_at_empty( info ) :-
	print_message( informational, spuds(no_srv) ).
spuds_stop_at_empty( quiet ).

help_at( Host, Port, Topic ) :-
	term_to_atom( Topic, Atopic) ,
	atomic_list_concat( ['http://',Host,':',Port,'/search?in=all&match=summary&for=',Atopic], Url ), 
	www_open_url( Url ),
	print_message( informational,spuds(www_browser(look_at(Url))) ).

ensure_server_is_running( Host, Port ) :-
	atomic_list_concat( ['http://',Host,':',Port], Url ), 
	catch( http_get(Url,_Reply,[]), _, fail ),
	!.
ensure_server_is_running( _Host, Port ) :-
	spuds_start_server( Port ).

spuds_start_server :-
	spuds_def( port, Port ),
	spuds_start_server( Port ).

spuds_start_server( _Port ) :-
	spuds_profile_file( ProFile ),
	absolute_file_name( pack(spuds), SpudsD ),
	directory_file_path( SpudsD, server, SrvD ),
	spuds_srv_basename( Bname ),
	directory_file_path( SrvD, Bname, SwiDoc ),
	process_create( path(chmod), ['u+x',SwiDoc], [] ),
	spuds_log_file( Logfile ),
	atomic_list_concat( [SwiDoc,ProFile,Logfile,'&'], ' ', BgScript ),
	% atom_concat( SwiDoc, ' &', BgScript ),
	shell( BgScript ),
	sleep( 3 ).

ps_swipl_rows_with( Pattern, Matched ) :-
	ps_rows( Rows ),
	findall( Row,  ( member(Row,Rows), 
	                   once( (   arg(_I,Row,ArgI),
				              sub_atom(ArgI,_Beg,_Len,_Aft,Pattern)
				          ) ),
						arg( 9, Row, '-q' ),
						arg( 10, Row, '-f' ),
						arg( 8,Row,swipl)
				),
				  	Matched ).

ps_rows( Rows ) :-
	tmp_file_stream( text, File, Stream ),
     close( Stream ),
	process_create( path(ps), ['--columns','200','-Af'],  [stdout(pipe(Pipe))] ),
	open( File, write, Out ),
	copy_stream_data( Pipe, Out ),
	close( Out ),
	close( Pipe ),
	read_ps_lines( File, Rows ).

read_ps_lines( File, CsvRows ) :-
	ReadOpts = [separator(0' ),strip(true),match_arity(false)],
	csv_read_file( File, [_Hdr|CsvRows], ReadOpts ).

row_is_unique( [Row], _Pattern, Row ) :- !.
row_is_unique( Matched, Pattern, _Row ) :-
	length( Matched, Len ),
	print_message( error, spuds(multiple_lines_found_for(Pattern,Len)) ), 
	% foreach( member(R,Matched), (write(R),nl) ),
	abort.

kill( Pid, Signal ) :-
	print_message( informational, spuds(killing(Pid,Signal)) ),
	process_create( path(kill), [Signal,Pid], [] ),
	sleep( 1 ).

/** spuds_profile_file( -ProFile ).

	Locates user's Profile file.

*/
spuds_profile_file( ProFile ) :-
	current_predicate( user:spuds_profile/1 ),
	user:spuds_profile( ProFile ),
	!.
spuds_profile_file( ProFile ) :-
	File = '$HOME/.pl/spuds_profile.pl',
     expand_file_name( File, [Default] ),
	file_name_extension( Stem, pl, Default ),
	spuds_profile_stem( Stem, ProFile ).

spuds_profile_stem( Stem, Profile ) :-
	gethostname( GotHost ),
	atomic_list_concat( [Host|_], '.', GotHost ),
	atomic_list_concat( [Stem,Host], '_', ProStem ),
	file_name_extension( ProStem, pl, Profile ),
	exists_file( Profile ),
	!.
spuds_profile_stem( Stem, Profile ) :-
	file_name_extension( Stem, pl, Profile ).
	% exists_file( Profile ). % fixme: add warning here if file does not exist

spuds_def( Key, Value ) :-
	spuds_profile_file( ProFile ),
     exists_file( ProFile ),
	% fixme: make sure we don't get garbage
	ensure_loaded( ProFile ),
	doc_server_default( Key, Value ),
     !.
spuds_def( Key, Value ) :-
	sys_doc_server_default( Key, Value ).
	
sys_doc_server_default( port, 4001 ).

% spuds_log_file( Log ).
%
%  Just a placeholder for now. 
%  As far as i know we store nothing important there.
%  In future we can inspect http:logfile to allow for user input that way.
%
spuds_log_file( '/tmp/spuds_log.txt' ).

:- multifile prolog:message/3.

prolog:message( error(Message) ) -->
	{print_message(error,Message)},
	{abort}.

prolog:message(spuds(Message)) -->
	message(Message).

message( killing(Pid,Signal) ) -->
	['Sending to process: ~w, signal: ~w' - [Pid,Signal] ].
message( multiple_lines_found_for(Pattern,Len) ) -->
	['Multiple lines (n=~d) for pattern: ~w.' - [Len,Pattern] ].
message(www_browser(look_at(Port))) -->
	[ 'Started browser on page ~w'-[Port] ].
message( too_many_srvs_to_kill(These) ) -->
	[ 'Too many spuds servers around :~w...'-[These], nl,
	  'Refusing to kill.' ].
message( no_srv ) -->
	[ 'Cannot verify any running spuds servers via ps.' - [] ].
message( srv_exists(Pid) ) -->
	['Cannot start server, as one already exists (pid=~d).' - [Pid] ].
message( no_spuds_pid(Pid) ) -->
	[ 'Cannot verify spuds server with pid:~w...'-[Pid], nl,
	  'Refusing to kill.' ].
message( spuds_splash ) -->
	[ 'Use, spuds(Topic), to start help topics from any Prolog session.' ].
