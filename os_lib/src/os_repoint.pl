/** os_repoint( +Link, +Target ).

Repoint an existing Link to a new Target.
Pred fails if Link exists but not a link.
Pred is debug(os_repoint) aware.

==
?- debug( os_repoint ).
?- shell( 'touch atzoumbalos' ).
?- shell( 'ln -s atzoumbalos shortos' ).
?- shell( 'touch atzoukos ).
?- os_repoint( shortos, atzoukos ).
% Warning, repointing link did not exist. Creating: shortos
% Linked to: '/home/nicos/pl/packs/private/os/atzoukos'
?- os_repoint( shortolos, atzoumbalos ).
% Repointing existing link: shortolos
% Old target was: '/home/nicos/pl/packs/private/os/atzoukos'
% Linked to: '/home/nicos/pl/packs/private/os/atzoumbalos'
?- os_repoint( danglink, atzou ).
% Warning, repointing link did not exist. Creating: danglink
% Linked to: '/home/nicos/pl/packs/private/os/atzou'
?- exists_file( danglink ).
false.
?- os_exists( danglink ).
false.
?- os_exists( danglink, type(flink) ).
false.
?- os_exists( danglink, type(link) ).
true.
==

@author nicos angelopoulos
@version  0.1 2014/7/23
@tbd extend interface to control link_file/3 3rd argument?
@see was repoint_link/2

*/
os_repoint( Link, Target ) :-
	os_abs( Link, Abs ),
	os_abs( Target, AbsTarget ),
	os_exists( Abs ),
	!,
	debug( os_repoint, 'Repointing existing link: ~p', Link ),
	os_repoint_existing( Link, AbsTarget ).
os_repoint( Link, Target ) :-
	debug( os_repoint, 'Warning, repointing link did not exist. Creating: ~p', Link ),
	os_abs( Link, Abs ),
	os_abs( Target, AbsTarget ),
	link_file( AbsTarget, Abs, symbolic ),
	debug( os_repoint, 'Linked to: ~p', AbsTarget ).

os_repoint_existing( Link, NewTarget ) :-
	read_link( Link, _, OldTarget ),
	!,
	debug( os_repoint, 'Old target was: ~p', OldTarget ),
	delete_file( Link ),
	link_file( NewTarget, Link, symbolic ),
	debug( os_repoint, 'Linked to: ~p', NewTarget ).
os_repoint_existing( Link, _NewTarget ) :-
	debug( os_repoint, 'Failure: target is not a link: ~p', Link ),
	fail.
