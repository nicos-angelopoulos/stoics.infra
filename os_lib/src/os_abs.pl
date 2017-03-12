/** os_abs( +Os, -Abs ).

Short for absolute_file_name/2 but also when Os is '' it is not interpreted as '.'. 
Os can a / starting slash Os term (os_name/2).
Note that absolute_file_name/2 deals correctly with all other os_name/2 types.

*/
os_abs( '', '' ) :- !.
os_abs( /(Sub), Abs ) :-
	!,
	os_term( Atom, Sub ),
	atomic_concat( '/', Atom, AbsSub ),
	absolute_file_name( AbsSub, Abs ).
os_abs( Rel, Abs ) :-
	absolute_file_name( Rel, Abs ).
