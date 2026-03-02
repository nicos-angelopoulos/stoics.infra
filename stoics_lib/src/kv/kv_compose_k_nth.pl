/** kv_compose_k_nth(+Terms, +N, -KVs).

Compose KVs by adding a K- to each Term in Terms list, by selecting the Nth element in Term.

Works for lists of Terms of either (sub) lists or compound terms. These can be mixed.
N numbering starts at 1 as per nth1/3.

Examples
==
?- kv_compose_k_nth([[1,2,3],[a,b,c]], 2, KVs).
KVs = [2-[1, 2, 3], b-[a, b, c]].

?- kv_compose_k_nth([no(1,2,3),let(a,b,c)], 2, KVs).
KVs = [2-no(1, 2, 3), b-let(a, b, c)].


?- kv_compose_k_nth([[1,2,3],let(a,b,c)], 2, KVs).
KVs = [2-[1, 2, 3], b-let(a, b, c)].

==

@author nicos angelopoulos
@version  0.1 2026/03/02
@tbd currently predicate fails with no error messaging if input is unexpected.

*/

kv_compose_k_nth( [], _N, [] ).
kv_compose_k_nth( [L|Ls], N, [Key-L|KVs] ) :-
     ( is_list(L) ->
          nth1( N, L, Key )
          ;
          arg( N, L, Key )
     ),
     kv_compose_k_nth( Ls, N, KVs ).
