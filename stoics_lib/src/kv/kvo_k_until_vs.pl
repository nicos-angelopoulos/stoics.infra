:- use_module(library(lists)).          % memberchk/2.

/** kvo_k_until_vs(+KVo, +UntilK, -Vs).
    kvo_k_until_vs(+KVo, +UntilK, -Vs, +Opts).

Select all the value elements for KVo (ordered Key-Value) pairs for which K is at most equal to UntilK. 

Note that KVo can contain multiple entries for a single Key.
==
?- kvo_k_until_vs([1-a,2-b,2-beta,3-c], 2, Vs ).
Vs = [a, b, beta].
==

The predicate works for any terms in KVo with arity of at least 2, and only the first two arguments 
are involved by default.

==
?- kvo_k_until_vs([t(1,a,v),t(2,b,x),t(2,beta,y),t(3,c,z)], 2, Vs ).
Vs = [a, b, beta].
==

Opts should be a single term or a list of terms from the following list.
Only the first occurance of a term is taken into account.

Opts
  * ki(Kindex=1)
    the argument position for Keys
  * vi(Vindex=2)
    the argument position for Values
  * compare(Comp=number)
    or =|term|= to change the comparison operator

The predicate uses the =< operator by default, but can be used on terms with option =|compare(term)|=
or if the first key is non-numeric.
==
?- kvo_k_until_vs([a-1,b-2,beta-2,c-3], beta, Vs, compare(term) ).
Vs = [1, 2, 2].

?- kvo_k_until_vs([a-1,b-2,beta-2,c-3], beta, Vs ).
Vs = [1, 2, 2].
==

The default positions at which KVo member terms hold the K and the V can be changed with options =|ki|= and =|vi|=.
KVo should be sorted on the key argument for the result to be correct.
==
?- kvo_k_until_vs([t1(v,a,1),t2(x,b,2),t3(y,b,2),t4(z,c,3)], b, Vs, [ki(2),vi(1)] ).
Vs = [v, x, y].
==

The more sensible uses for non standard Ki and Vi, is (a) when Ki remains =|1|= and a different argument
is picked up by Vi, as in
==
?- kvo_k_until_vs([t(a,1,alpha),t(b,2,beta),t(c,3,gamma)], b, Vs, vi(3)).
Vs = [alpha, beta].
==
or (b) when there is an invariant first =|N|= arguments in the KVo as in 
==
?- kvo_k_until_vs([t(x,1,alpha),t(x,2,beta),t(x,3,gamma)], 2, Vs, [ki(2),vi(3)]).
Vs = [alpha, beta].
==

The implementation defines two distinct sub-predicates, one for each comparison type- 
in order to keep execution as efficient as possible.

The predicate fails if Opts is not ground.

@author nicos angelopoulos
@version  0:1 2026/3/5

*/
kvo_k_until_vs( KVo, MxK, Vs ) :-
     kvo_k_until_vs( KVo, MxK, Vs, [] ).

kvo_k_until_vs( KVo, MxK, Vs, ArgS ) :-
     ground( ArgS ),
     ( ArgS = [_|_] -> ArgS = Args ; Args = [ArgS] ),
     Defs = [ki(1),vi(2)], % compare(number) is the default; 
                           % not needed in the list as  checked as anything else than compare(term)
     append( Args, Defs, Opts ),
     memberchk( ki(Ki), Opts ),
     memberchk( vi(Vi), Opts ),
     ( kvo_k_until_vs_op( KVo, Ki, Opts ) ->
          kvo_k_until_vs_terms( KVo, MxK, Ki, Vi, Vs )
          ;
          kvo_k_until_vs_numbs( KVo, MxK, Ki, Vi, Vs )
     ).

kvo_k_until_vs_op( _KVo, _Ki, Opts ) :-
    memberchk( compare(term), Opts ),
    !.
kvo_k_until_vs_op( KVo, Ki, _Opts ) :-
     KVo = [KV|_],
     arg( Ki, KV, Key ),
     \+ number(Key).
     

kvo_k_until_vs_terms( [], _MxK, _Ki, _Vi, [] ).
kvo_k_until_vs_terms( [H|T], MxK, Ki, Vi, Vs ) :-
     arg( Ki, H, Kh ),
     ( Kh @=< MxK ->
          arg( Vi, H, V ),
          Vs = [V|TVs],
          R = T
          ;
          R = [],
          Vs = TVs
     ),
     kvo_k_until_vs_terms( R, MxK, Ki, Vi, TVs ).

kvo_k_until_vs_numbs( [], _MxK, _Ki, _Vi, [] ).
kvo_k_until_vs_numbs( [H|T], MxK, Ki, Vi, Vs ) :-
     arg( Ki, H, Kh ),
     ( Kh =< MxK ->
          arg( Vi, H, V ),
          Vs = [V|TVs],
          R = T
          ;
          R = [],
          Vs = TVs
     ),
     kvo_k_until_vs_numbs( R, MxK, Ki, Vi, TVs ).
