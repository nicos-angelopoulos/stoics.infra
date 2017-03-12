%% position_type( +Data, -Dtype ).
%
%  Dtype is the determined datatype for Data.
%  If atomic(Data) succeeds, Dtype is _atomic_.
%  If Dtype is not a variable and it unifies [_|_],
%  then Dtype unifies _list_, Otherwise, 
%  Dtype is _compound_.
%
position_type( Data, Dtype ) :-
	atomic( Data ),
	!,
	Dtype = atomic.
position_type( Data, Dtype ) :-
	\+ var( Data ),
	Data = [_|_],
	!,
	Dtype = list.
position_type( _Data, Dtype ) :-
	% test \+ var( Data ),  ??
	Dtype = compound.
