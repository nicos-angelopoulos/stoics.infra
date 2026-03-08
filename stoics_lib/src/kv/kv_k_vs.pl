:- use_module(library(lists)).          % reverse/2, append/3.
:- use_module(library(ordsets)).        % ord_add_element/3.
:- use_module(library(prolog_pack)).    % pack_property/2.

:- lib(promise(options_append/4,options)).

:- lib(stoics_lib:nth1/5).
:- lib(stoics_lib:kvs_k_update_v/7).

kv_k_vs_defaults( Defs ) :-
             Defs = [
                      conseq(false),
                      ki(1),
                      sort_k(true),
                      sort_vs(true),
                      vi(2)
                      | Checks
                      ],
             ( pack_property(pack_errors,version(_V)) -> 
                    Checks = [options_types([conseq-boolean,ki-integer,sort_k-boolean,sort_vs-boolean,vi-integer])]
                    ;
                    Checks = []
             ).

/** kv_k_vs(+KVs, -KGvs).
    kv_k_vs(+KVs, -KGvs, +Opts).

Aggregate Vs based on common Key within terms KVs.

kv_compose_k_frequency/3 requires pack(options), wheres kv_compose_k_frequency/2 
works fine without it.

Opts
  * conseq(Csq=false)
    wether to group only consequtive occurances
  * fun(Fun)
    functor for KGvs (default is that of KVs)
  * ki(Kindex=1)
    the argument position for Keys
  * sort_k(SortK=true)
    whether the result should be ordered, or as they come in input (only effective if =|Csq=false|=
  * sort_vs(SortVs=false)
    whether the joined Vs are ordered
  * vi(Vindex=2)
    the argument position for Values

Most of the ways to achive the groupings achieved here can be also done via 
group_pairs_by_key/2 and kv_compose_k_frequency/3. 
These will require a pass through keysort/2 or sort/2,4 whereas here we do a native one pass 
implementation, which is likely to be slower overall. 

This predicate provides a common interface and also, in comparison to group_pairs_by_key/2 is more permissive on allowed KVs and
constructed KGvs.

==
?- kv_k_vs( [1-a,2-b,3-c], KGvs ).
KGvs = [1-[a], 2-[b], 3-[c]].

?- kv_k_vs( [1-a,2-b,3-c,1-alpha,2-beta,3-gamma], KGvs ).
KGvs = [1-[a, alpha], 2-[b, beta], 3-[c, gamma]].
==

==
?- kv_k_vs( [1-a,2-b,3-c,1-alpha,2-beta,3-gamma], KGvs, [ki(2),vi(1),fun(+)] ).
KGvs = [a+[1], alpha+[1], b+[2], beta+[2], c+[3], gamma+[3]].

?- kv_k_vs( [2-beta,1-alpha,3-gamma,1-a,2-b,3-c], KGvs ).
KGvs = [1-[a, alpha], 2-[b, beta], 3-[c, gamma]].

?- kv_k_vs( [2-beta,1-alpha,3-gamma,1-a,2-b,3-c], KGvs, sort_k(false) ).
KGvs = [2-[b, beta], 1-[a, alpha], 3-[c, gamma]].

?- kv_k_vs( [2-beta,1-alpha,3-gamma,1-a,2-b,3-c], KGvs, [sort_k(false),sort_vs(false)] ).
KGvs = [2-[beta, b], 1-[alpha, a], 3-[gamma, c]].
==

@author nicos angelopoulos
@version  0:1 2026/3/8
@see group_pairs_by_key/2 and kv_compose_k_frequency/3.

*/
kv_k_vs( KVs, KGvs ) :-
     KVs = [KV|_KVt],
     KV =.. [Fun,_K,_V|_],
     kv_k_vs_univ( KVs, 1, 2, Fun, true, [], KGvs ).

kv_k_vs( KVs, KGvs, Args ) :-
     Self = kv_k_vs,
     lib_promised( options_append/4, stoics_lib, kv_k_vs/3 ),
     OAOpts = [pack(stoics_lib),arity(3)],
     options_append( Self, Args, Opts, OAOpts ),
     options( [ki(Ki),vi(Vi)], Opts ),
     ( memberchk(fun(Fun),Opts) -> true; KVs = [KV|_], functor(KV,Fun,_Art) ), % fixme: check Art >= 2
     options( conseq(Csq), Opts ), 
     options( sort_k(SoK), Opts ),
     options( sort_vs(SoV), Opts ),
     kv_k_vs( Csq, SoK, SoV, KVs, Ki, Vi, Fun, KGvs ).

kv_k_vs( true, _SoK, SoV, KVs, Ki, Vi, Fun, KGvs ) :-
     !,
     KVs = [KV|TKVs],
     arg( Ki, KV, K ),
     arg( Vi, KV, V ),
     kv_k_vs_conseq( TKVs, K, 1, [V], Ki, Vi, Fun, SoV, KGvs ).
kv_k_vs( false, false, SoV, KVs, Ki, Vi, Fun, KGvs ) :-
     !,
     kv_k_vs_uni_unord( KVs, SoV, Ki, Vi, Fun, [], KGvs ).
kv_k_vs( false, true, SoV, KVs, Ki, Vi, Fun, KGvs ) :-
     kv_k_vs_univ( KVs, Ki, Vi, Fun, SoV, [] , KGvs ).

kv_k_vs_uni_unord( [], _SoV, _Ki, _Vi, _Fun, AccKGvs, KGvs ) :-
     reverse( AccKGvs, KGvs ).
kv_k_vs_uni_unord( [KV|KVs], SoV, Ki, Vi, Fun, AccKGvs, KGvs ) :-
     arg( Ki, KV, K ),
     arg( Vi, KV, V ),
     CurKV =.. [Fun,K,CurVs], 
     NxtKV =.. [Fun,K,NxtVs],
     % we 
     ( nth1(_N,AccKGvs,NxtKV,CurKV,NxtKGvs) ->
          kv_k_vs_uni_unord_upd_vs( SoV, CurVs, V, NxtVs )
          ;
          NxtKGvs = [K-[V]|AccKGvs]
     ),
     kv_k_vs_uni_unord( KVs, SoV, Ki, Vi, Fun, NxtKGvs, KGvs ).

kv_k_vs_uni_unord_upd_vs( true, CurVs, V, NxtVs ) :-
     ord_add_element( CurVs, V, NxtVs ).
kv_k_vs_uni_unord_upd_vs( false, CurVs, V, NxtVs ) :-
     % difference lists ? (we can use nth1/5's 1st argument- on an auxiliary list of tails
     append( CurVs, [V], NxtVs ).

kv_k_vs_univ( [], _Ki, _Vi, _Fun, _Ovs, AccKGvs, KGvs ) :-
     AccKGvs = KGvs.
     % kvs_k_update_v( AccKGvs, K, kv_k_ord_v_new(Fun,V), kv_k_ord_vs_upd(Ovs,Fun,V), _OldV, _NewV, KGvs  ).
     % it will be expensive to get the unsorted vs in this version- maybe a list pointing to difference lists
kv_k_vs_univ( [KV|KVs], Ki, Vi, Fun, Ovs, AccKGvs, KGvs ) :-
     arg( Ki, KV, K ),
     arg( Vi, KV, V ),
     kvs_k_update_v( AccKGvs, K, kv_k_ord_v_new(Fun,V), kv_k_ord_vs_upd(Ovs,Fun,V), _OldV, _NewV, NxtKGvs  ),
     kv_k_vs_univ( KVs, Ki, Vi, Fun, Ovs, NxtKGvs, KGvs ).

% for now we don't pass Ovs in kv_k_ord_v_new/n, as it doesn't matter, both cases still a list on V.
kv_k_ord_v_new( Fun, V, K, NewV, NewKGvs ) :-
     NewV = [V],
     NewKGvs =.. [Fun,K,[V]].
kv_k_ord_vs_upd( true, Fun, V, K, OldV, NewV, NewKV ) :-
     ord_add_element( OldV, V, NewV ),
     NewKV =.. [Fun,K,NewV].
kv_k_ord_vs_upd( false, Fun, K, V, OldV, NewV, NewKV ) :-
     append( OldV, [V], NewV ),
     NewKV =.. [Fun,K,NewV].

kv_k_vs_conseq( [], K, AccVs, _Ki, _Vi, Fun, _SoV, KGvs ) :-
     KV =.. [Fun,K,AccVs],
     KGvs = [KV].
kv_k_vs_conseq( [KV|KVs], K, AccVs, Ki, Vi, Fun, SoV, KGvs ) :-
     arg( Ki, KV, K1 ),
     arg( Ki, KV, V1 ),
     ( K1 == K -> 
          ( SoV == true ->
               ord_add_element( AccVs, V1, NxtVs )
               ;
               append( AccVs, [V1], NxtVs )
          ),
          KGvs = TKGvs
          ;
          NxtVs = [V1],
          NxtKV =.. [Fun,K,AccVs],
          KGvs = [NxtKV|TKGvs]
     ),
     kv_k_vs_conseq( KVs, K1, NxtVs, Ki, Vi, Fun, SoV, TKGvs ).
