
:- lib(stoics_lib_throw/2).

break_nth_defaults( [at_short(throw)] ).

/* break_nth( ?N, +List, -Left, -Right ).
   break_nth( ?N, +List, -Left, ?Opts ).

Split List on Nth Position, producing Left.

If the 4th argment is a variable is instantiated to the remainder of the list.
v0.2 introduced Opts as the 4th argument. Calls should be backward compatible, but new calls should use 4th arg as Options list.

First element position is 1. Nth element is last element in Left.

Opts
  * at_short(AtShort=throw)
    what to do when List is shorter than N
    * throw()
      throws an error (pack_errors?)
    * fail()
      silently fail
    * write()
      writes message and fails
    * true
      succeeds, returning the full list in Left and [] to Right
  * remainder(Right)
    remainder of the operation. List = Left + Right

==

?- break_nth( 0, [a,b,c], L, R ).  L=[], R=[a,b,c]
?- break_nth( 1, [a,b,c], L, R ).  L=[a], R=[b,c]
?- break_nth( 3, [a,b,c], L, R ).  L=[a,b,c], R=[].

% when pack_errors is installed.
?- break_nth( 4, [a,b,c], L, R ).
ERROR: stoics_lib:break_nth/4: Object of type: list longer than 4, expected but found term: list of length, 3

% when pack_errors not installed.
?- break_nth( 4, [a,b,c], L, R ).
ERROR: Unhandled exception: Unknown message: type_error('list longer than 4','list of length, 3')+(stoics_lib:break_nth/4)

?- break_nth( N, [a,b,c], L, R ).
N = 1,
L = [a],
R = [b, c] ;
N = 2,
L = [a, b],
R = [c] ;
N = 3,
L = [a, b, c],
R = [] ;
false.
==

@author nicos angelopoulos
@version  0.2 added 4th arg as Opts and throw for error instead of write out

*/
break_nth( N, List, Left, Fourth ) :-
	Self = break_nth,
     ( var(Fourth) -> 
          Args = [remainder(Fourth)]
          ;
          Fourth = Args
     ),
     options_append( Self, Args, Opts ),
     options( at_short(AtShort), Opts ),
     ErrOpt = stoics_lib:Self/4,
     ( var(N) ->
          break_nth_gen( N, List, Left, Right )
          ;
	     ( N < 0 ->
               % fixme: use must_be or pack_error
               stoics_lib_throw( arg_natural_number(N,Self), ErrOpt )
		     ;
		     ( N =:= 0 ->
			     Left = [],
			     Right = List
			     ;
     		     length( List, Length ),
			     ( Length =:= N ->
				     Left = List, Right = []
				     ;
				     ( Length < N ->
                              break_nth_err( AtShort, List, Left, Right, N, Length, ErrOpt )
					     ;
     				     break_nth_1( N, List, Left, Right )
				     )
			     )
		     )
	     )
     ),
     ( memberchk(remainder(Right),Opts) -> true; true ).

break_nth_1( 1, [H|T], [H], T ) :- !.
% break_nth_for_list_1( 1, [X|Xs], [X], Xs ) :- !.
break_nth_1( N, [X|Xs], [X|Ls], Right ) :-
     N1 is N - 1,
     break_nth_1( N1, Xs, Ls, Right ).

break_nth_gen( 1, [H|T], [H], T ).
% break_nth_for_list_1( 1, [X|Xs], [X], Xs ) :- !.
break_nth_gen( N, [X|Xs], [X|Ls], Right ) :-
     break_nth_gen( N1, Xs, Ls, Right ),
     N is N1 + 1.

break_nth_err( throw, _List, _Left, _Right, N, Length, ErrOpts ) :-
     atom_concat( 'list longer than ', N, Arg1 ),
     atom_concat( 'list of length, ', Length, Arg2 ),
     stoics_lib_throw( type_error(1,Arg1,Arg2), ErrOpts ).
     % write( user_error, list_of_insufficient_length(legth(Length),limit(N),Self) ), nl( user_error ), abort
break_nth_err( fail, _List, _Left, _Right, _N, _Length, _ErrOpts ) :-
     fail.
break_nth_err( write, _List, _Left, _Right, N, Length, ErrOpts ) :-
     Format = 'In ~w. List of longer than: ~d positions expected, the provided had length: ~d',
     Args = [ErrOpts,N,Length],
     message_report( Format, Args, error ),
     fail.
break_nth_err( true, List, Left, Right, _N, _Length, _ErrOpt ) :-
     Right = [],
     List = Left.
