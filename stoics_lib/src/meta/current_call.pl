
:- lib(goal_spec/2).

/** current_call( +Goal ).
    current_call( +Goal, +Else ).

If Goal's predicate indicator is defined, call Goal.
Otherwise, call Else, if in current_call/2, or fail if we are in current_call/1.

==
?- current_call( irrelevant(x) ).
false.
?- current_call( irrelevant(x), true ).
true.

% be cautious of auto_loading
?- current_call( member(X,[a,b,c]) ).
false.

?- member(X,[a,b,c]).
X = a ;
X = b ;
X = c.

?- current_call( member(X,[a,b,c]) ).
X = a ;
X = b ;
X = c.

==

 @author nicos angelopoulos
 @version  0.1 2014/9/14
 @tbd interact with autoloading

*/
current_call( Goal ) :-
	current_call( Goal, fail ).

current_call( Goal, _Else ) :-
	goal_spec( Goal, Spec ),
	current_predicate( Spec ),
	!,
	call( Goal ).
current_call( _Goal, Else ) :-
	call( Else ).
