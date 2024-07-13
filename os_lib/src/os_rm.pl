
/** os_rm(Os).

See os_remove/1.

*/
os_rm( Os ) :-
	os_remove( Os, [] ).
os_rm( Os, Opts ) :-
	os_remove( Os, Opts ).
