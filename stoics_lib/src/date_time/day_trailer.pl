/** day_trailer( +Day, -AtomTrailer ).

Get the AtomTrailer corresponding to Day (of the month).

==
?- day_trailer( 2, Scn ).


?- day_trailer( 15, Ftn ).

==

*/
day_trailer(  1, 'st' ) :- !.
day_trailer(  2, 'nd' ) :- !.
day_trailer(  3, 'rd' ) :- !.
day_trailer( 21, 'st' ) :- !.
day_trailer( 22, 'nd' ) :- !.
day_trailer( 23, 'rd' ) :- !.
day_trailer( 31, 'st' ) :- !.
day_trailer( _AllOther, 'th' ).
