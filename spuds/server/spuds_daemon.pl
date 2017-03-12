
:- use_module( library(http/http_unix_daemon) ).
:- use_module( library(doc_http) ).
:- use_module( library(spuds) ).
:- ensure_loaded( library(spuds_docs) ).

:- listen( http(post_server_start), spuds_daemon_listener ).
:- set_prolog_flag(lib_initialise,false).

/*
   to start from the command line do something like: 

	swipl -l spuds_daemon.pl -- --port=4004

   if your server fails to start  with /etc/init.d/swipl-spuds
   it might be due to one of your source code files that fails to load properly.
   As far as i know, spuds tries to catch exceptions and failures, but in the 
   context of the server, SWI doesnot behave as expected (reported, not fixed).
   To see if this is the case, change at the system location you have placed spuds_daemon.pl 
   comment out the initialization line at the end, start 

	% swipl -f none 

	?- set_prolog_flag( spuds_start, false ).
	?- [spuds_daemon].
	?- debug( spuds ).
   ?- http_daemon( [port(4004),fork(false)] ).

	[See file packs(spuds(server(spuds_debug.pl))).]
	Debugging add messages to /tmp/repo.txt.
   Make sure you correct any errors coming out of this before rerunning the server.
   
   These are older notes: 
   to use debugging start server with 
   sudo /etc/init.d/swipl-spuds stop
   sudo /etc/init.d/swipl-spuds debug

   we should add levels here
   	sections, files
	all = true  
*/
load_verbosity( true ).

spuds_daemon_listener :-
	current_prolog_flag(argv, Argv),
	http_unix_daemon:argv_options(Argv, _RestArgv, Options),
	( (memberchk(spuds_debug(Dbg),Options),Dbg==true) -> debug(spuds); true ),
	spuds_profile_file( Profile ),
	load_user_file( Profile ),
	spuds_docs.

spuds_daemon_initialiase :-
	current_prolog_flag( spuds_start, false ),
	!,
	debug( _, 'Spuds server not started because spuds_start flag was set to false.', true ).

spuds_daemon_initialiase :-
	all_sources_visible( true ),
	http_daemon.

% :- http_handler( /, pldoc, [prefix] ). % works for non daemonised stufff

:- initialization( spuds_daemon_initialiase ).
