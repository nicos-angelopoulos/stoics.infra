/** get_datetime( Dtime ).

Get current datime as a datetime/6 term structure.

== 
?- get_datetime( Dime ).
Dime = datetime(2016, 12, 2, 10, 42, 26).
==

@version 0.1   2016/12/02 (some time well before).
@author  nicos angelopoulos

*/
get_datetime( Dtime ) :-
	get_time(T),stamp_date_time(T,Date,local), 
	Date = date(Y,M,D,H,N,S,_,_,_),
	Secs is integer(S),
	Dtime = datetime(Y,M,D,H,N,Secs).
