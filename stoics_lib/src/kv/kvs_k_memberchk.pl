%% kvs_k_memberchk( +K, +KVset, -V ).
% 
%  Select the unique V corresponding to 
%  Should this be covered by a unification version of Swi's ord_memberchk/2 ?
%  It seems counter intuitive that they are using ==.
%
%  Should there be a kvo version?  This assumes unique keys in addition to sorted.
%
%==
% kvs_k_memberchk( b, [a-1,b-2,c-3], V ).
% V = 2.
%
% kvs_k_memberchk( d, [a-1,b-2,c-3], V ).
% false.
%
% kvs_k_memberchk( c, [a+1,b+2,c+3], V ).
% V = 3.
%
% kvs_k_memberchk( b, [a-1,b-2,b-4,c-3], V ).
% V = 2.
%
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/5/23
% @version  0.2 2018/1/8,   generalised via arg(1,,) & arg(2,,) to non - terms
%
kvs_k_memberchk( K, [KV|KVs], V ) :-
    arg( 1, KV, Kh ),
    arg( 2, KV, Vh ),
    compare( Ord, Kh, K ),
	kvs_k_memberchk_compared( Ord, Vh, KVs, K, V ).

kvs_k_memberchk_compared( <, _OldVh, OldT, K, V ) :-
    OldT = [KV|T],
    arg( 1, KV, Kh ),
    arg( 2, KV, Vh ),
    compare( Ord, Kh, K ),
    kvs_k_memberchk_compared( Ord, Vh, T, K, V ).
kvs_k_memberchk_compared( =, Vh, _T, _K, Vh ).
