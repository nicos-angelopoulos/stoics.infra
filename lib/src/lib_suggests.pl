/** lib_suggests( +LibS ).

	For a library or list of libraries
	load the libraries if they are available, else be quiet.

*/
lib_suggests( Libs ) :-
	is_list( Libs ),
	!,
	maplist( lib_suggests, Libs ).
lib_suggests( Lib ) :-
	% % prolog_load_context( module, LoadMod ),
	% that's a bit strange... but seems need to add LoadMod
	% below for it to be used from command line...
	% % catch( LoadMod:ensure_loaded(library(Lib)), _, true ).
	% fixme: be more specific in the catch ?
    % lib( Lib, [mode(suggests)] ).
    catch( lib(Lib,[]), _, fail ),
    !.
lib_suggests( Lib ) :-  % fixme:
    debug( lib, 'Suggested repository no installed: ~w', Lib ).
