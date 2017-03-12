%% compare_arithmetic( -Op, +X, +Y ).
%
% As compare, but using arithmetic operations. 
%
%==
% ?- compare( Op, 3, 3.0 ).
% Op = (>).
% 
% ?- compare_arithmetic( Op, 3, 3.0 ).
% Op = (=).
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/2/16
%
compare_arithmetic( Op, X, Y ) :-
	( X =:= Y -> 
		Op = =
		;
		( X < Y ->
			Op = <
			;
			Op = >
		)
	).
