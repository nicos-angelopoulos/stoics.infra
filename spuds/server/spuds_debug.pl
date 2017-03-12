% use this to debug the spuds server...
% load with swipl -f spuds_debug so no personal Prolog is loaded
% 
spuds_debug :-
	set_prolog_flag( spuds_start, false ),
	ensure_loaded( spuds_daemon ),
	debug( spuds ),
	http_daemon( [port(4004),fork(false)] ).

:- initialization( spuds_debug ).
