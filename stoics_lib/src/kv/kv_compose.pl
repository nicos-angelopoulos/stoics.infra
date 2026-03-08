
:- use_module(library(lists)).     % memberchk/2.

:- lib(promise(options_append/4,options)).

kv_compose_defaults( Defs ) :-
                         Defs = [
                                   fun('-'),
                                   ki(0),
                                   single_input(false),
                                   vi(0)
                         ].

/** kv_compose( +Ks, +Vs, -KVs ).
    kv_compose( -Ks, -Vs, +KVs ).
    kv_compose( +Ks, +Vs, -KVs, +Opts ).

Construct arity two terms by selecting terms from input list(s).

In kv_compose/3 version Ks and Vs are lists and KVs are -(K,V) pairs. 
This version can be used to also de-construct a KVs to its constituent lists.
The inner implementation of this is the text book one, and it is faster than
when bespoke options are used.
==
?- kv_compose([a,b,c], [1,2,3], KVs).
KVs = [a-1, b-2, c-3].

?- kv_compose(Ks, Vs, [a-1,b-2,c-3]).
Ks = [a, b, c],
Vs = [1, 2, 3].
==

Arity 4 version depends on pack(options), whereas arity 3 one works fine without it.

In kv_compose/4 version provides some flexibility on how to construct the KVs. 
Ks and Vs can be lists of terms from which specific arguments can be selected
or two arguments from a single list of terms can be selected. 
User can also define the functor of the KV terms and ask for them to be a difference list.
Opts should be a single term or a list of terms from the following (only the first instance
of a specific term is taken into cosideration).

Opts
  * fun('-')
    functor for KV pairs
  * ki(Kindex=0)
    the argument position for Keys (=|0|= means whole term)
  * single_input(Sin=false)
    wheter to take both Key and Values from Ks
  * vi(Vindex=0)
    the argument position for Values (=|0|= means whole term)
  * tail(Tail)
    make KVs a difference list with Tail as its tail

==
?- kv_compose([a,b,c], [1,2,3], KVs, tail(X)).
KVs = [a-1, b-2, c-3|X].

?- kv_compose([k(a),l(b),m(c)], [1,2,3], KVs, ki(1) ).
KVs = [a-1, b-2, c-3].

?- kv_compose([k(a,1),l(b,2),m(c,3)], false, KVs, [ki(1),vi(2),fun(+),single_input(true),tail(Y)] ).
KVs = [a+1, b+2, c+3|Y].

?- kv_compose([a,b,c], _, KVs, single_input(true)).
KVs = [a-a, b-b, c-c].
==

To keep things efficient, the predicate implements four different constructor predicates.
If all Opts are in default values, the vanilla kv_compose/3 is used. If tail(Tai) is given
but all other Opts are in default, then an extra argument is added to the vanilla implementation.
Otherwise, two flexible version are used, depending on single input on not, and which are bound to be somewhat slower.

@author nicos angelopoulos
@version  0.2 2017/2/24    added /4 version.
@version  0.3 2026/3/7     changed 4th arg to Opts and added all options functionality

*/
kv_compose( Ks, Vs, KVs, Args ) :-
     Self = kv_compose,
     OAOpts = [pack(stoics_lib),arity(4)],
     lib_promised( options_append/4, stoics_lib, kv_compose/4 ),
     options_append( Self, Args, Opts, OAOpts ),
     options( [ki(Ki),vi(Vi)], Opts ),
     options( single_input(Sin), Opts ),
     options( fun(Fun), Opts ),
     ( (Ki=:=0,Vi=:=0,Sin==false,Fun=='-') ->
                         ( memberchk(tail(Tail1),Opts) ->
                                        kv_compose_tail(Ks, Vs, KVs, Tail1)
                                        ;
                                        kv_compose(Ks, Vs, KVs)
                         )
                         ;
                         ( memberchk(tail(Tail2),Opts) -> true; Tail2 = [] ),
                         ( Sin == true ->
                              kv_compose_sin(Ks, Ki, Vi, Fun, KVs, Tail2)
                              ;
                              kv_compose_full(Ks, Vs, Ki, Vi, Fun, KVs, Tail2)
                         )
     ).

kv_compose( [], [], [] ).
kv_compose( [Hk|Tks], [Hv|Tvs], [Hk-Hv|Tkvs] ) :-
	kv_compose( Tks, Tvs, Tkvs ).

kv_compose_tail( [], [], Tail, Tail ).
kv_compose_tail( [Hk|Tks], [Hv|Tvs], [Hk-Hv|Tkvs], Tail ) :-
    kv_compose_tail( Tks, Tvs, Tkvs, Tail ).

kv_compose_sin( [], _Ki, _Vi, _Fun, KVs, Tail ) :-
     KVs = Tail.
kv_compose_sin( [H|T], Ki, Vi, Fun, [KV|KVs], Tail ) :-
     ( Ki =:= 0 -> K = H; arg(Ki, H, K) ),
     ( Vi =:= 0 -> V = H; arg(Vi, H, V) ),
     KV =.. [Fun,K,V],
     kv_compose_sin( T, Ki, Vi, Fun, KVs, Tail ).

kv_compose_full( [], [], _Ki, _Vi, _Fun, KVs, Tail ) :-
     KVs = Tail.
kv_compose_full( [Kt|Ks], [Vt|Vs], Ki, Vi, Fun, [KV|KVs], Tail ) :-
     ( Ki =:= 0 -> K = Kt; arg(Ki, Kt, K) ),
     ( Vi =:= 0 -> V = Vt; arg(Vi, Vt, V) ),
     KV =.. [Fun,K,V],
     kv_compose_full( Ks, Vs, Ki, Vi, Fun, KVs, Tail ).
