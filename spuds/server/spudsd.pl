:- use_module(library(pldoc/doc_library)).
:- catch( use_module(library(ansi_term)), _, true ).

:- ensure_loaded( library(spuds_docs) ).

:- use_module(library(http/http_log)).
:- use_module(library(settings)).

:- on_signal(hup, _, hup).

:- dynamic(swi_doc_seen/1).

:- set_prolog_flag( gui, false ).
:- set_prolog_flag( xpce, false ).

init :-
	current_prolog_flag( argv, Args ),
	append( _, [DcsF,LogF], Args ),
	!,
	init( DcsF, LogF ).
init :-
	gethostname( FullHost ),
	atomic_list_concat( [Host|_], '.', FullHost ),
	atomic_list_concat( [spuds_profile,Host], '_', WithHost ),
	expand_file_name( '$HOME/.pl', [HomePL] ),
	member( ProfStem, [WithHost,spuds_profile] ),
	file_name_extension( ProfStem, pl, ProFile ),
	directory_file_path( HomePL, ProFile, AbsFile ),
	exists_file( AbsFile ),
	!,
	init( AbsFile, '/tmp/spuds_log.txt' ).
init :-
	expand_file_name( '$HOME/.pl/spuds_profile.pl', [Profile] ),
	init( Profile, '/tmp/spuds_log.txt' ).

init( DcsF, LogF ) :-
	set_setting( http:logfile, LogF ),
	% set_prolog_flag( verbose_load, true ),  % testing only
	set_prolog_flag( verbose, normal ),
	( exists_file(DcsF) -> load_user_file( DcsF ) ; true ),           % fixme: warning
	( (current_predicate(spuds_debug/1),spuds_debug(true)) ->
		set_prolog_flag( verbose_load, true),
		assert( load_verbosity(false) )
		;
		set_prolog_flag( verbose_load, false ),
		assert( load_verbosity(true) )
	).

spudsd :-
	doc_server_port( Port ),
	spudsd( Port ).

spudsd( Port ) :-
	atomic_list_concat( ['http://',localhost,':',Port], Url ), 
	catch( http_get(Url,_Reply,[]), _, fail ),
	!,
	print_message( informational, spuds(srv_exists(Port) ) ).
spudsd( Port ) :-
	current_prolog_flag_fs( gui, Gui ), 
	set_prolog_flag( gui, false ),
     doc_server( Port ),
		 % in constrainted environments the above does not get set ?!
	spuds_docs,
	set_prolog_flag( gui, Gui ),
     thread_get_message(stop),
	halt.

current_prolog_flag_fs( Flag, Value ) :-
	current_prolog_flag( Flag, Value ),
	!.
current_prolog_flag_fs( _Flag, false ).

doc_server_port( Port ) :-
	current_predicate( doc_server_default/2 ),
	doc_server_default( port, Port ),
	!.
doc_server_port( 4001 ).

hup(_Signal) :-
     thread_send_message(main, stop).

:- multifile prolog:message/3.
prolog:message( spuds(srv_exists(Port)) ) -->
	[ 'Server seems to be already running at port:~w.' - [Port], nl,
	  'Goodbye' ].

% :- initialization( (spudsd) ).
:- initialization( (init,spudsd) ).
