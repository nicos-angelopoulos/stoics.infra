
:- lib(options).

ex_app_defaults( [opt1(true)] ).

/** ex_app.
    ex_app( +Opts ).

Tests atoms(Atoms) and foreign(Frg) options of options_append/4.

==
?- ex_app.
atoms([a,b,c])
opts([frg1(false),opt1(false),opt1(true)])
foreign([frg1(false)])
==

@author nicos angelopoulos
@version  1.0 2016/12/10

*/

ex_app :- 
	ex_app( [frg1(false),opt1(false),a,b,c] ).

ex_app( Args ) :-
	options_append( ex_app, Args, Opts, [atoms(Atoms),foreign(Frg)] ),
	write( atoms(Atoms) ), nl,
	write( opts(Opts) ), nl,
	write( foreign(Frg) ), nl.
