/** portray_clauses( +List, +OptS ).

Record a bunch of clauses to either a stream or file. Supports append and write modes.
OptS can be a list or single option term from the following:

Opts

* mode(Mode=append)
  append or write

* stream(Stream)
  default is user_output

* file(File)
  if present, overwrites Stream. if pack(by_unix) is present File will be passed
  through by_unix_term_to_serial/2 before passed to open/3

@author nicos angelopoulos
@verison  0.1 2016/12/10,  modified for public release

*/

:- lib(en_list/2).
:- lib(current_call/2).
:- lib(suggests(by_unix)).

portray_clauses( List, Opt ) :-
     en_list( Opt, Opts ),
     ( memberchk(file(FilePrv),Opts) ->
          ( memberchk(mode(Mode),Opts) -> true; Mode = write ),
		current_call( by_unix_term_to_serial(FilePrv,File), File=FilePrv ),
          open( File, Mode, Out )
          ;
          ( memberchk(stream(Out),Opts) -> true; Out= user_output )
     ),
     portray_clauses_onto( List, Out ),
     ( var(File) -> true; close( Out ) ).

portray_clauses_onto( [], _ ).
portray_clauses_onto( [H|T], Out ) :-
	portray_clause( Out, H ),
	portray_clauses_onto( T, Out ).
