
:- multifile message_hook/3.

message_hook( error(w_instantiation_error,where(N/A,Var)), error, Lines ) :-
     append( Lines, [' @ ',N,'/',A,', ',Var], App ),
     '$messages':print_system_message( term, error, App ).
