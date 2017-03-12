%% en_list( +Term, -Listed ).
%
% Ensure that Term is either a list of things or 
% a non-var term that is wrapped to a singleton list.
% If In is a variable a ball is thrown.
%
%==
% ?- en_list( x(y), Opts ).
% Opts = [x(y)].
%
% ?- en_list( [x,y], Opts ).
% Opts = [x, y].
%
% ?- en_list( X, Opts ).
% ERROR: Unhandled exception: en_list(encoutered_variable_in_1st_arg(_))
%==
% 
%@author  nicos angelopoulos
%@version   0.2   2016/12/10
%
en_list( In, Opts ) :-
	is_list( In ),
	!,
	Opts = In.
en_list( In, Opts ) :-
	\+ var( In ),
	!,
	Opts = [In].
en_list( Else, _Opts ) :-
	throw( en_list(2,encountered_variable_in_1st_arg(Else)) ).
