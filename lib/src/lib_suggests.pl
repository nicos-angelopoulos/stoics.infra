
lib_suggests_defaults( [mode(suggests),suggest(false)] ).

/** lib_suggests( +LibS ).
    lib_suggests( +LibS, +Opts ).

	Load a library or list of libraries, if they are available, else be quiet.<br>
    Opts are passed to lib/2. lib_suggests/2 by default passes mode(suggests) and <br>
    suggest(false) which differ to lib/2 defaults. If you want to override the local<br>
    defaults, pass appropriate values in Opts
    
@author nicos angelopoulos
@version  0.2 2018/01/24, added /2 verion, clean up and more docs
@tbd example

*/
lib_suggests( Libs ) :-
    lib_suggests( Libs, [] ).

lib_suggests( LibS, ArgS ) :-
    lib_en_list( LibS, Libs ),
    lib_en_list( ArgS, Args ),
	!,
    lib_suggests_defaults( Defs ),
    append( Args, Defs, Opts ),
	maplist( lib_suggests_opts(Opts), Libs ).

lib_suggests_opts( Opts, Lib ) :-
    % catch( lib(Lib,Opts), _, fail ),
    lib( Lib, Opts ),
    !.
lib_suggests_opts( Lib ) :-  % fixme:
    debug( lib, 'Suggested repository not installed: ~w', Lib ).
