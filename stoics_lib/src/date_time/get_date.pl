
:- lib(get_date_time/1).

%% get_date( -Date ).
%
%  Get the current date in date/1 format.
%  Tested on Swi, not in Yap.
%
% @author nicos angelopoulos
% @version  0.1
% @see get_date_time/1
%
get_date( date(Y,M,D) ) :-
	get_date_time( date(Y,M,D,_,_,_,_,_,_) ).
