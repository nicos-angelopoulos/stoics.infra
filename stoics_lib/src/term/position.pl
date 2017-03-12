
:- lib( position_type/2 ).

%% position( ?N, +Data, ?Nth ).
%% position( +Type, ?N, +Data, ?Nth ).
%% position( +Type, ?N, +Data, ?Nth, -NxN, -Cont ).
%
%  An experimental polymorphic predicate that works on Data that is one of,
%  list, compound, number or atom. When atomic only position 1 is valid.
%  Cont is the most efficient structure for continuing enumerating Data.
%  In the case of lists, this is the list minus the Nth element and for everything
%  else, Cont is unified to Data. NxN is the next counter for Cont, 
%  when Type is list, that is 1 until at the end of the list when it 0, else is N + 1.
%  The main idea behind NxN and Cont is to provide support for iterators.
%  The loop can end when NxN is equal to either 0 or to arity(Data).
%  
% ==
%  position( 2, [1,2,3], W ).
%  position( 2, c(1,2,3), W ).
%  position( compound, 2, c(1,2,3), W ).
%  position( list, 2, c(1,2,3), W ).
%  position( list, 2, c(1,2,3), W ).
%
% ?- position( list, 1, [1,2,3,4], Nth, NxN, Cont ).
% == 
% @author  nicos angelopoulos
% @version 0.1 2014/02/09
% @version 0.2 2014/06/30 switch to term_type/2.
%
position( N, Data, Nth ) :-
	position_type( Data, Dtype ),
	position( Dtype, N, Data, Nth ).

position( atomic, 1, Data, Data ).
position( list, N, Data, Nth ) :-
	nth1( N, Data, Nth ).
position( compound, N, Data, Nth ) :-
	arg( N, Data, Nth ).

position( list, N, Data, Nth, NxN, Cont ) :-
	( N =:= 1 -> Data = [Nth|Cont] ; nth1(N,Data,Nth,Cont) ),
	( Cont == [] -> NxN is 0; NxN is 1 ).
position( compound, N, Data, Nth, NxN, Data ) :-
	arg( N, Data, Nth ),
	NxN is N + 1.
position( atomic, 1, Data, Data, 2, Data ).
