
:- use_module(library(prolog_pack)).    % pack_property/2.
:- use_module(library(lists)).          % reverse/2.

:- lib(promise(options_append/4,options)).

kv_compose_k_frequency_defaults( Defs ) :-
                                   Defs = [
                                             drop_f(false),
                                             drop_k(false),
                                             drop_v(false),
                                             keysort(true),
                                             ki(1),
                                             vi(2)
                                             | Checks
                                          ],
                                   ( pack_property(pack_errors,version(_V)) -> 
                                        Checks = [options_types([drop_f-boolean,drop_k-boolean,keysort-boolean])]
                                        ;
                                        Checks = []
                                   ).

/** kv_compose_k_frequency(+KVs, -FKVs).
    kv_compose_k_frequency(+KVs, -FKVs, +Opts).

Aggregate  Key-Value terms KVs, on Frequency of Keys.

The resulting FKVs are of the form F-K-Vs, where Vs is a list of V elements.
FKVs are returned in sorted order for K as per keysort/2 and Vs are 
in the order they appear in KVs as keysort/2 does not use them for sorting.  
The predicate uses a first pass through keysort/2 (or sort/4 if non K-V input is given, see below),
before it walks through the input KVs.
It can be used to return the groups only (see option drop_f(DropF)).

kv_compose_k_frequency/3 requires pack(options), whereas kv_compose_k_frequency/2 
works fine without it.

Opts
  * drop_f(Dropf=false)
    set to =|true|= to return the K-Groups only
  * drop_k(DropK=false)
    whether to drop K from answer
  * drop_v(DropV=false)
    whether to drop Vs from answer
  * keysort(Ksort=true)
    whether to use keysort, (otherwise sort/2 is used creating total order)
  * ki(Kindex=1)
    the argument position for Keys
  * vi(Vindex=2)
    the argument position for Values


Examples
==
?- kv_compose_k_frequency([1-y,2-b,1-x,1-a], FKVs).
FKVs = [3-1-[y, x, a], 1-2-[b]].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs), keysort( FKVs, FKVo ).
FKVs = [2-1-[x, a], 1-2-b],
FKVo = [1-2-b, 2-1-[x, a]].
==

kv_compose_k_frequency/3 examples
==
?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, []). 
FKVs = [2-1-[x, a], 1-2-[b]].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, keysort(false)).
FKVs = [2-1-[a, x], 1-2-[b]].


?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, drop_k(true)). 
FKVs = [2-[x, a], 1-[b]].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, drop_v(true)).
FKVs = [2-1, 1-2].

?- kv_compose_k_frequency([2-b,1-x,1-a], FKVs, [drop_k(true),drop_v(true)]).
FKVs = [2, 1].

?- lib(stoics_lib:kv_compose_k_nth/3).
?- kv_compose_k_nth([no(1,2,3),no(4,2,6),no(7,8,9)], 2, KVs), kv_compose_k_frequency(KVs, FKVs, drop_k(true)).
KVs = [2-no(1, 2, 3), 2-no(4, 2, 6), 8-no(7, 8, 9)],
FKVs = [2-[no(1, 2, 3), no(4, 2, 6)], 1-[no(7, 8, 9)]].

This predicate can be used to return the groupings only.
==
?- kv_compose_k_frequency([1-y,2-b,1-x,1-a], FKVs, drop_f(true)).
FKVs = [1-[y, x, a], 2-[b]].
?- kv_compose_k_frequency([1-y,2-b,1-x,1-a], FKVs, [drop_f(true),keysort(false)]).
FKVs = [1-[a, x, y], 2-[b]].
==

The predicate can extract frequency and KVs on more general input structures by passing it options ki(Ki) and vi(Vi).
==
?- Abet = [lt('1st',a,en),lt('1st',alpha,gr),lt('2nd',b,en),lt('2nd',beta,gr),lt('3rd',c,en),lt('3rd',gamma,gr)], assert(a_bet(Abet)).
?- a_bet(Abet), kv_compose_k_frequency(Abet,FKVs,[ki(1),vi(2)]).
FKVs = [2-'1st'-[a, alpha], 2-'2nd'-[b, beta], 2-'3rd'-[c, gamma]].

?- a_bet(Abet), kv_compose_k_frequency(Abet,FKVs,[ki(3),vi(2)]).
FKVs = [3-en-[a, b, c], 3-gr-[alpha, beta, gamma]].
==

If pack(pack_errors) is installed, (the predicate just checks, it does not require the pack): 
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
     ( KVs = [_-_|_] -> keysort( KVs, KVo )
                     ;     sort( KVs, KVo )
     ),
     KVo = [KV|KVoT],
     arg( 1, KV, K ),
     arg( 2, KV, V ),
     kv_compose_k_frequency_order( KVoT, K, 1, [V], 1, 2, falsefalsefalse, FKVs ).

kv_compose_k_frequency( KVs, FKVs, Args ) :-
     Self = kv_compose_k_frequency,
     lib_promised( options_append/4, stoics_lib, kv_compose_k_frequency/3 ),
     OAOpts = [pack(stoics_lib),arity(3)],
     options_append( Self, Args, Opts, OAOpts ),
     options( keysort(Sort), Opts ),
     options( [ki(Ki),vi(Vi)], Opts ),
     kv_compose_k_frequency_option_value( Sort, keysort ),
     kv_compose_k_frequency_sort( Sort, Ki, KVs, KVo ),
     options( drop_f(Df), Opts ),
     options( drop_k(Dk), Opts ),
     options( drop_v(Dv), Opts ),
     kv_compose_k_frequency_option_value( Df, drop_f ),
     kv_compose_k_frequency_option_value( Dk, drop_k ),
     kv_compose_k_frequency_option_value( Dv, drop_v ),
     atomic_list_concat( [Df,Dk,Dv], '', Dfkv ),
     KVo = [KV|KVoT],
     arg( Ki, KV, K ),
     arg( Vi, KV, V ),
     kv_compose_k_frequency_order( KVoT, K, 1, [V], Ki, Vi, Dfkv, FKVs ).

kv_compose_k_frequency_sort(true, Ki, KVs, KVo) :-
     ( KVs = [_-_|_] -> 
          keysort( KVs, KVo )
          ;
          sort( Ki, @=<, KVs, KVo )
     ).
kv_compose_k_frequency_sort(false, _Ki, KVs, KVo) :-
     sort( KVs, KVo ).

kv_compose_k_frequency_order( [], K, Fa, V, _Ki, _Vi, Dfkv, [FKV] ) :-
     reverse( V, ReV ),
     kv_compose_k_frequency_drop_k_v( Dfkv, Fa, K, ReV, FKV ).
     
kv_compose_k_frequency_order( [KV1|KVs], K, Fa, V, Ki, Vi, Dfkv, FKVs ) :-
     arg( Ki, KV1, K1 ),
     arg( Vi, KV1, V1 ),
     ( K1 == K -> 
          NxV = [V1|V],
          Fx is Fa + 1,
          TFKVs = FKVs
          ; % we are assuming K1 cannot be < K
          reverse( V, ReV ),
          kv_compose_k_frequency_drop_k_v( Dfkv, Fa, K, ReV, FKV ),
          FKVs = [FKV|TFKVs],
          Fx is 1,
          NxV = [V1]
     ),
     kv_compose_k_frequency_order( KVs, K1, Fx, NxV, Ki, Vi, Dfkv, TFKVs ).

kv_compose_k_frequency_drop_k_v(truetruetrue, _F, _K, _V, []).
kv_compose_k_frequency_drop_k_v(truetruefalse, _F, _K, V, V).
kv_compose_k_frequency_drop_k_v(truefalsetrue, _F, K, _V, K).
kv_compose_k_frequency_drop_k_v(truefalsefalse, _F, K, V, K-V).
kv_compose_k_frequency_drop_k_v(falsetruetrue, F, _K, _V, F).
kv_compose_k_frequency_drop_k_v(falsetruefalse, F, _K, V, F-V).
kv_compose_k_frequency_drop_k_v(falsefalsetrue, F, K, _V, F-K).
kv_compose_k_frequency_drop_k_v(falsefalsefalse, F, K, V, F-K-V).

kv_compose_k_frequency_option_value(true, _OptNm) :- !.
kv_compose_k_frequency_option_value(false, _OptNm) :- !.
kv_compose_k_frequency_option_value(Etc, OptNm) :-
     throw( type_error(for_option(OptNm),should_be(boolean),found(Etc)) ).
