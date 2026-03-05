
:- use_module(library(prolog_pack)).    % pack_property/2.
:- use_module(library(lists)).          % reverse/2.

kv_compose_k_frequency_defaults( Defs ) :-
                                   Defs = [
                                             drop_k(false),
                                             drop_v(false),
                                             keysort(true)
                                             | Checks
                                          ],
                                   ( pack_property(pack_errors,version(_V)) -> 
                                        Checks = [options_types([drop_k-boolean,keysort-boolean])]
                                        ;
                                        Checks = []
                                   ).

/** kv_compose_k_frequency(+KVs, -FKVs).
    kv_compose_k_frequency(+KVs, -FKVs, +Opts).

Aggregate on Frequency of Keys in Key-Value terms KVs.

The resulting FKVs are of the form F-K-Vs, where Vs is a list of Vs iff F > 1.
FKVs are returned in sorted order for K as per keysort/2 and Vs are 
in the order they appear in KVs as keysort/2 does not use them for sorting.

kv_compose_k_frequency/3 requires pack(options), wheres kv_compose_k_frequency/2 
works fine without it.

Opts
  * drop_k(DropK=false)
    whether to drop K from answer
  * drop_v(DropV=false)
    whether to drop Vs from answer
  * keysort(Ksort=true)
    whether to use keysort, instead of sort/2

Examples
==
?- kv_compose_k_frequency([1-a,1-x,2-b], FKVs).
FKVs = [2-1-[a, x], 1-2-b].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs).
FKVs = [2-1-[x, a], 1-2-b].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs), keysort( FKVs, FKVo ).
FKVs = [2-1-[x, a], 1-2-b],
FKVo = [1-2-b, 2-1-[x, a]].
==

kv_compose_k_frequency/3 examples
==
?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, []). 
FKVs = [2-1-[x, a], 1-2-b].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, keysort(false)).
FKVs = [2-1-[a, x], 1-2-b].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, [drop_k(true)]). 
FKVs = [2-[x, a], 1-b].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, [drop_v(true)]).
FKVs = [2-1, 1-2].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, [drop_k(true),drop_v(true)]).
FKVs = [2, 1].

?- lib(stoics_lib:kv_compose_k_nth/3).
?- kv_compose_k_nth([no(1,2,3),no(4,2,6),no(7,8,9)], 2, KVs), kv_compose_k_frequency( KVs, FKVs, drop_k(true) ).
KVs = [2-no(1, 2, 3), 2-no(4, 2, 6), 8-no(7, 8, 9)],
FKVs = [2-[no(1, 2, 3), no(4, 2, 6)], 1-no(7, 8, 9)].


If pack(pack_errors) is installed (the predicate just checks, it does not require the pack): 
==
?- kv_compose_k_frequency([1-a,1-x,2-b], FKVs, [keysort(maybe)]).
ERROR: stoics_lib:kv_compose_k_frequency/3 @ option(keysort): Object of type: boolean expected, but found term: maybe.
==

If pack(pack_errors) is not installed: 
==
?- kv_compose_k_frequency([1-a,1-x,2-b], FKVs, [keysort(maybe)]).
ERROR: Unhandled exception: Unknown message: type_error(for_option(keysort),should_be(boolean),found(maybe))
==

@author nicos angelopoulos
@version  0.1 2026/03/02
*/

kv_compose_k_frequency( KVs, FKVs ) :-
     keysort( KVs, KVo ),   % built-in
     KVo = [K-V|KVoT],
     kv_compose_k_frequency_order( KVoT, K, 1, V, falsefalse, FKVs ).

kv_compose_k_frequency( KVs, FKVs, Args ) :-
     lib( options ),
     Self = kv_compose_k_frequency,
     OAOpts = [pack(stoics_lib),arity(3)],
     options_append( Self, Args, Opts, OAOpts ),
     options( keysort(Sort), Opts ),
     kv_compose_k_frequency_option_value( Sort, keysort ),
     kv_compose_k_frequency_sort( Sort, KVs, KVo ),
     options( drop_k(Dk), Opts ),
     options( drop_v(Dv), Opts ),
     atom_concat( Dk, Dv, DkDv ),
     kv_compose_k_frequency_option_value( Dk, drop_k ),
     kv_compose_k_frequency_option_value( Dv, drop_v ),
     KVo = [K-V|KVoT],
     kv_compose_k_frequency_order( KVoT, K, 1, V, DkDv, FKVs ).

kv_compose_k_frequency_sort(true, KVs, KVo) :-
     keysort( KVs, KVo ).
kv_compose_k_frequency_sort(false, KVs, KVo) :-
     sort( KVs, KVo ).

kv_compose_k_frequency_order( [], K, Fa, V, DkDv, [FKV] ) :-
     ( Fa =:= 1 ->
          ReV = V
          ;
          reverse( V, ReV )
     ),
     kv_compose_k_frequency_drop_k_v( DkDv, Fa, K, ReV, FKV ).
     
kv_compose_k_frequency_order( [K1-V1|KVs], K, Fa, V, DkDv, FKVs ) :-
     ( K1 == K -> 
          ( Fa =:= 1 -> 
               NxV = [V1,V]  % we will be reversing
               ;
               NxV = [V1|V]
          ),
          Fx is Fa + 1,
          TFKVs = FKVs
          ; % we are assuming K1 cannot be < K
          ( Fa =:= 1 ->
               V = ReV 
               ;
               reverse( V, ReV )
          ),
          kv_compose_k_frequency_drop_k_v( DkDv, Fa, K, ReV, FKV ),
          FKVs = [FKV|TFKVs],
          Fx is 1,
          NxV = V1
     ),
     kv_compose_k_frequency_order( KVs, K1, Fx, NxV, DkDv, TFKVs ).

kv_compose_k_frequency_drop_k_v(truetrue, Fa, _K, _V, Fa).
kv_compose_k_frequency_drop_k_v(truefalse, Fa, _K, V, Fa-V).
kv_compose_k_frequency_drop_k_v(falsetrue, Fa, K, _V, Fa-K).
kv_compose_k_frequency_drop_k_v(falsefalse, Fa, K, V, Fa-K-V).

kv_compose_k_frequency_option_value(true, _OptNm) :- !.
kv_compose_k_frequency_option_value(false, _OptNm) :- !.
kv_compose_k_frequency_option_value(Etc, OptNm) :-
     throw( type_error(for_option(OptNm),should_be(boolean),found(Etc)) ).
