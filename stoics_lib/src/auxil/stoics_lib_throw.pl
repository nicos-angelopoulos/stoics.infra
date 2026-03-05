/** stoics_lib_throw(+Err,+Opts).

Internal predicate. Makes dependancy to pack_errors a suggestion.

@author nicos angelopoulos
@version  0.1 2024/10/18

*/
stoics_lib_throw( Term, Opts ) :-
     ( current_predicate(throw/2) ->
          throw( Term, Opts )
          ;
          throw( Term+Opts )
     ).
