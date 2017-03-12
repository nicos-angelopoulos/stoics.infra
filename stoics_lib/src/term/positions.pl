:- lib( position_type/2 ).

%% positions( +Data, -Dtype, -NofPositions ).
%% positions( +Data, -NofPositions ).
%
%  Number of positions and data type for list/compound Data.
% If Data is a list NofPositions is the length. If Data is atomic
% the length is 1, and otherwise the number of positions
% is its arity. Dtype is correspondingly, _list_ and  _compound_.
% 
%==
%   positions( [1,2,3,4], P ).
%==
% 
% @author nicos angelopoulos
% @version  0.1 2014/02/09
% @tbd  allow for data()  (see my compound preds).
%
positions( Data, Positions ) :-
	positions( Data, _Dtype, Positions ).

positions( Data, Dtype, Positions ) :-
	position_type( Data, Dtype ),
	positions_type( Dtype, Data, Positions ).

positions_type( number(_), _Data, 1 ).
positions_type( atomic, _Data, 1 ).
positions_type( list, Data, Positions ) :-
	length( Data, Positions ).
positions_type( compound, Data, Positions ) :-
	functor( Data, _Name, Positions ).
