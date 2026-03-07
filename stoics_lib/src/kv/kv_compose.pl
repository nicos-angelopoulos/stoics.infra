% :- lib(depends(kv_compose/4,pack(options)).

/** kv_compose( +Ks, +Vs, -KVs, +Opts ).
kv_compose( Ks, Vs, KVs, Args ) :-
     lib( options ),
     Self = kv_compose,
     OAOpts = [pack(stoics_lib),arity(4)],
     options_append( Self, Args, Opts, OAOpts ),
*/

/** kv_compose( +Ks, +Vs, -KVs ).
    kv_compose( +Ks, +Vs, -KVsCont, -Tkvs ).

Ks and Vs are lists and KVs and KVsCont are made of -pairs of their values.
Tkvs is the tail of difference list KVsCont.

==
?- kv_compose( [a,b,c], [1,2,3], Kvs ).

==

@author nicos angelopoulos
@version  0.2 2017/2/24    added /4 version.

*/
kv_compose( [], [], [] ).
kv_compose( [Hk|Tks], [Hv|Tvs], [Hk-Hv|Tkvs] ) :-
	kv_compose( Tks, Tvs, Tkvs ).

kv_compose( [], [], Tail, Tail ).
kv_compose( [Hk|Tks], [Hv|Tvs], [Hk-Hv|Tkvs], Tail ) :-
    kv_compose( Tks, Tvs, Tkvs, Tail ).
