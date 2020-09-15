/** io_streams( ?In, ?Out, ?Error ).

Generalises set_prolog_IO/3 with enquiry mode.

Modes can be mixed, eg: 

==
?- 
   io_streams( In, user_output, Error1 ),
   io_streams( In, Out, Error2 ).

In = <stream>(0x7fdc8665e780),
Error1 = Error2, Error2 = <stream>(0x7fdc8665e980),
Out = <stream>(0x7fdc8665e880).

==
@author nicos angelopoulos
@version  0:1 2020/9/15

*/
io_streams( In, Ou, Er ) :-
    I is 0,
    ( var(In) ->
        stream_property( In, alias(user_input) ),
        J is I + 1
        ;
        J is I
    ),
    ( var(Ou) -> 
        stream_property( Ou, alias(user_output) ),
        K is J + 1
        ;
        K is J
    ),
    ( var(Er) ->
        stream_property( Er, alias(user_error) ),
        L is K + 1
        ;
        L is K
    ),
    ( L < 3 ->
        set_prolog_IO( In, Ou, Er )
        ;
        true
    ).


