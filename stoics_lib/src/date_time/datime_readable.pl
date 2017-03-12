
:- lib(int_trailer/2).
:- lib(day_trailer/2).
:- lib(get_date_time/1).
:- lib(three_letter_months/1).

/** datime_readable( -Ratom ).
    datime_readable( +Datime, -Ratom ).

Ratom is a human readable representation of Datime.
When Datime is missing the current datime is used.

==
?- datime_readable( Readable ).
Readable = 'At 15:13:36 on 2nd of Jul 2014'.
==

@author nicos angelopoulos
@version  0.2 2014/7/2  Changed to date/9 and atom representation. Be ware if you are using 0.1
@see debug_goal/3
@tbd add precision for seconds.

*/
datime_readable( Readable ) :-
	get_date_time( Current ),
	datime_readable( Current, Readable ).

datime_readable( date(Year,Month,Day,Hour,Mins,SecPrec,_,_,_), Readable ) :-
	three_letter_months( SymbolicMonths ),
	nth1( Month, SymbolicMonths, SymbMonth ),
	day_trailer( Day, Trailer ),
	Sec is floor( SecPrec ),
	Parts = ['At ',Hour,':',Mins,':',Sec,' on ',Day,Trailer,' of ',SymbMonth,' ',Year],
	atomic_list_concat( Parts, Readable ).
