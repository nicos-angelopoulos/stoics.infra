
:- use_module(library(lists)).     % append/3.

options_rename_defaults([debug(false),replace(true)]).

/** options_rename(+Opts, +RnmPairS, -Newts, +RnmOpts).

Rename options with new functors in Opts resulting in Newts.

RnmPairS is a single or list of pairs of the form OptNm-NewNm.

RnmOpts
  * debug(Dbg=false)
    informational, progress messages
  * replace(Rplc=true)
    by default predicate removes options that match New functors

Examples
==
?- 
     options_rename([a(1),b(2),c(3),b(4)], b-d, New, true).

New = [a(1), d(2), c(3), d(4)].

?-  
     options_rename([a(1),b(2),c(3)], b-c, New,  true ).

New = [a(1), c(2)].

?-  
     options_rename([a(1),b(2),c(3)], b-c, New,  replace(false) ).

New = [a(1), c(2), c(3)].
==

@author nicos angelopoulos
@version  0.1 2023/09/24

*/

options_rename( OptsIn, PairS, Newts, Args ) :-
     Self = options_rename,
     options_append( Self, Args, Opts ),
     options( replace(Rpc), Opts ),
     options_en_list( OptsIn, OptsL ),
     options_en_list( PairS, Pairs ),
     options_rename_1( OptsL, Pairs, Rpc, Newts ),
     debug( Self, 'Renamed options: ~w', [Newts] ).

options_rename_1( [], _Pairs, _Rpc, [] ).
options_rename_1( [Opt|Opts], Pairs, Rpc, Newts ) :-
     ( Opt =.. [Onm|Orgs] ->
          ( memberchk(Onm-Nnm,Pairs) ->
               Npt =.. [Nnm|Orgs],
               Newts = [Npt|Tewts]
               ; 
               ( memberchk(_-Onm,Pairs) ->
                    % old flag that matches one of the new namesreplace if flag hasn't changed to false
                    options_rename_replace( Rpc, Opt, Newts, Tewts )
                    ;
                    Newts = [Opt|Tewts]
               )
          )
          ;
          % pass through atoms, for instance
          Newts = [Opt|Tewts]
     ),
     options_rename_1( Opts, Pairs, Rpc, Tewts ).

options_rename_replace( false, Opt, Newts, Tewts )  :- 
     !,
     Newts = [Opt|Tewts].
options_rename_replace( _, _Opt, Newts, Newts ).   % defaulty
