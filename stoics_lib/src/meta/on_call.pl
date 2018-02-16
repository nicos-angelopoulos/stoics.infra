/** on_call( +OnB, +Call, +ArgIn, -ArgOut ),

A generic caller of Call iff OnB is _true_. <br>
The call incorporates as its two last args ArgIn and ArgOut.<br>
The predicate provides a simple way to control via an option (pack(options))
the call of a predicate on partial results. <br>
typically that is on the elements of an output list.

==
?- assert( to_integer(Num,Int) :- Int is integer(Num) ).
?- on_call( true, to_integer, 3.0, Three ).
Three = 3.
==

@author nicos angelopoulos
@version  0.1
@tbd add a on_call(Args,Body) option to options_append/4 that asserts temp preds (and means to clean them afterwards).

*/
on_call( false, _Goal, _In, _Out ).
on_call( true, Goal, In, Out ) :-
    call( Goal, In, Out ).
