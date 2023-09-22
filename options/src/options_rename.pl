
:- use_module(library(lists)).     % append/3.

options_rename_defaults([debug(false),rename_all(remove)]).

/** options_rename(+OptsL,+OptNm,+NewNm,-Newts,+Opts).

Rename option with functor OptNm in Opts to one with functor NewNm resulting in Newts.

Opts
  * debug(Dbg=false)
    informational, progress messages
  * rename_all(Rall=remove)
    in which case later mentions are removed, 
    or =true= for renaming all, and =false= for not renaming (faster)

Examples
==
?- 
     options_rename([a(1),b(2),c(3),b(4)],b,d,New,true).

New = [a(1), d(2), c(3)].

?-
     options_rename([a(1),b(2),c(3),b(4)],b,d,New,rename_all(false)).

New = [a(1), d(2), c(3), b(4)].

?-
     options_rename([a(1),b(2),c(3),b(4)],b,d,New,rename_all(true)).

New = [a(1), d(2), c(3), d(4)].

==

@author nicos angelopoulos
@version  0.1 2023/09/22

*/

options_rename( OptsIn, NmIn, NmOut, Newts, Args ) :-
     Self = options_rename,
     options_append( Self, Args, Opts ),
     options( rename_all(Rnm), Opts ),
     options_en_list( OptsIn, OptsL ),
     options_rename_1( OptsL, NmIn, NmOut, Rnm, Newts ),
     debug( Self, 'Renamed options: ~w', [Newts] ).

options_rename_1( [], _NmIn, _NmOut, _Rnm, [] ).
options_rename_1( [Opt|Opts], NmIn, NmOut, Rnm, Newts ) :-
     ( Opt =.. [NmIn|Orgs] ->
          Npt =.. [NmOut|Orgs],
          Newts = [Npt|Mewts],
          options_rename_continuation( Rnm, Opts, NmIn, NmOut, Mewts, RemOpts, Tewts )
          ;
          Newts = [Opt|Tewts],
          Opts = RemOpts
     ),
     options_rename_1( RemOpts, NmIn, NmOut, Rnm, Tewts ).

options_rename_continuation( false, Opts, _NmIn, _NmOut, Newts, RemOpts, Tewts ) :-
     RemOpts = [],
     append( Opts, Tewts, Newts ).
options_rename_continuation( remove, Opts, NmIn, _NmOut, Newts, RemOpts, Tewts ) :-
     RemOpts = [],
     options_rename_remove( Opts, NmIn, Rpts ),
     append( Rpts, Tewts, Newts ).
options_rename_continuation( true, Opts, _NmIn, _NmOut, Newts, RemOpts, Tewts ) :-
     RemOpts = Opts,
     Newts = Tewts.

options_rename_remove( [], _Onm, [] ).
options_rename_remove( [Opt|Opts], Onm, Rpts ) :-
     ( functor(Opt,Onm,_) -> 
          Rpts = Tpts
          ;
          Rpts = [Opt|Tpts]
     ),
     options_rename_remove( Opts, Onm, Tpts ).
