
lib_suggests_defaults( [mode(suggests),suggest(false),suggests_warns(false)] ).

/** lib_suggests( +LibS ).
    lib_suggests( +LibS, +Opts ).

	Load a library or list of libraries, if they are available, else be quiet.<br>
Opts are passed to lib/2. lib_suggests/2 by default passes mode(suggests), <br>
suggest(false) which differ to lib/2 defaults. It also sets suggests_warns(false).
If you want to override the local defaults, pass appropriate values in Opts.

By default missing suggested libraries are not reported.
This behaviour is controlled by prolog_flag/2 flag: _lib_suggests_warns_
  * auto
    (default) warn if developer has used suggests_warns(true) option
  * false
    do not print any warning messages
  * debug
    as _true_ but also print informational message on successful loading
  * true
    warn of any missing libs

==
?- lib(suggests(whato)).
true.

?- lib(suggests(whato,suggests_warns(true))).
% Failed to load suggested library:whato, in context: user
true.

?- set_prolog_flag( lib_suggests_warns, true ).
?- lib(suggests(whato)).
% Failed to load suggested library:whato, in context: user
true.

?- lib(real).
?- lib(r('GGally')).
true.
?- set_prolog_flag(lib_suggests_warns, auto).
?- lib( suggests(r('phantom')) ).   % a non-existing library
true.
?- set_prolog_flag(lib_suggests_warns, true).
?- lib( suggests(r('phantom')) ).   % a non-existing library
==

@author nicos angelopoulos
@version  0.2 2018/1/24, added /2 version, clean up and more docs
@version  0.3 2019/4/18, warning control via flag + option
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
    memberchk( mode(Mode), Opts ),
    memberchk( suggest(Sugg), Opts ),
    memberchk( suggests_warns(Warns), Opts ),
    LibOpts = [mode(Mode),suggest(Sugg),suggests_warns(Warns)],
	maplist( lib_suggests_opts(LibOpts), Libs ).

lib_suggests_opts( Opts, Lib ) :-
    % catch( lib(Lib,Opts), _, fail ),
    lib( Lib, Opts ),
    !.
lib_suggests_opts( _Opts, Lib ) :-  % fixme:
    debug( lib, 'Suggested library not installed: ~w', Lib ).
