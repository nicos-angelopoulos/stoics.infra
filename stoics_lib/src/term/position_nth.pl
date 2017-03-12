
:- lib(position_type/2).
% :- lib( term_type ).  % /2.

%% position_nth( +N, +Data, -Nth ).
%% position_nth( +N, +Data, -Nth, -Rem ).
%% position_nth( +N, +Data, -Nth, -Rem, -Nxt ).
%% position_nth( +Dtype, +N, +Data, -Nth, -Rem, -Nxt ).
%
% Get Data's N position datum into Nth, 
% with Rem being what is left of data and Nxt is the
% N identifier for the next to the _right_ of Nth.
% Predicate expects that bounds are respected, else fails.
% Dtype is the datatype of Data, either _list_ or 
% _compound_ which is determined by the predicate if missing.
%
% ==
%    Data = [1,2,3,4,5],
%    position_nth( list, 2, Data, Nth, Rem, Nxt ).
%    position_nth( compound, 2, Data, Nth, Rem, Nxt ).
%    position_nth( list, 1, Data, Nth, Rem, Nxt ).
%
% ?- maplist( position_nth(3), [c(1,2,3),c(4,5,6)], Thirds, Rem ).
% Thirds = [3, 6],
% Rem = [c(1, 2), c(4, 5)].
% ==
% @author  nicos angelopoulos
% @version 0.2, 2014/02/27 changed from position_next
% @see   position/4 for an iterator assistant
%
position_nth( N, Data, Nth ) :-
	% term_type( Data, Dtype ),
	position_type( Data, Dtype ),
	position_type_nth( Dtype, N, Data, Nth ).

position_nth( N, Data, Nth, Rem ) :-
	% term_type( Data, Dtype ),
	position_type( Data, Dtype ),
	position_nth( Dtype, N, Data, Nth, Rem, _Nxt ).

position_nth( N, Data, Nth, Rem, Nxt ) :-
	% term_type( Data, Dtype ),
	position_type( Data, Dtype ),
	position_nth( Dtype, N, Data, Nth, Rem, Nxt ).

position_nth( number(_), N, Data, Nth, Rem, N ) :-
	number_codes( Data, Codes ),
	position_nth_list( N, Codes, Nth, RemCodes, _Nxt ),
	number_codes( Rem, RemCodes ).
position_nth( atom, N, Data, Nth, Rem, N ) :-
	atom_codes( Data, Codes ),
	position_nth_list( N, Codes, Nth, RemCodes, _Nxt ),
	atom_codes( Rem, RemCodes ).
position_nth( list, N, Data, Nth, Rem, Nxt ) :-
	position_nth_list( N, Data, Nth, Rem, Nxt ).
position_nth( string, N, Data, Nth, Rem, N ) :-
	string_codes( Data,  Codes ),
	position_nth_list( N, Codes, Nth, RemCodes, _Nxt ),
	string_codes( Rem, RemCodes ).
position_nth( compound, N, Data, Nth, Rem, N ) :-
	nth_arg( N, Data, Nth, Rem ).

% special case
% position_nth_list( 1, [H|T], H, T, 2 ) :- !.
position_nth_list( N, List, Nth, Rest, N ) :-
	nth1( N, List, Nth, Rest ).

% check the modes, probably works fine for generating.
position_type_nth( list, N, Data, Nth ) :-
	nth1( N, Data, Nth ).
position_type_nth( number(_), 1, Datum, Datum ).
position_type_nth( atom, 1, Datum, Datum ).
position_type_nth( compound, N, Data, Nth ) :-
	arg( N, Data, Nth ).
