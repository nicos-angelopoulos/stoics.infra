%% kvo_k_memberchk( +Key, +KVord, -Val ).
% 
% Select each Val corresponding to a single Key in a ordered (but not uniquely sorted) pairs list KVord.<br>
% KVord can be a list of any N-ary terms, Key is taken to be the first arg/3 and Val the second.
%
% In contrast to kvs_k_memberchk/3, this assumes non-unique keys. <br>
% In both cases KVset is assumed ordered.
%
%==
% kvo_k_memberchk( b, [a-1,b-2,c-3], V ).      % compare to kvs_k_memberchk/3
% V = 2;
% false.
% 
% kvo_k_memberchk( b, [a-1,b-2,b-4,c-3], V ).
% V = 2;
% V = 4;
% false.
%
% kvo_k_memberchk( d, [a-1,b-2,c-3], V ).
% false.
%
% kvo_k_memberchk( c, [a+1,b+2,c+3], V ).
% V = 3;
% false.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/5/23
% @version  0.2 2018/1/8,   generalised via arg(1,,) & arg(2,,) to non - terms
%
kvo_k_memberchk( K, [KV|KVs], V ) :-
    arg( 1, KV, Kh ),
    arg( 2, KV, Vh ),
    compare( Ord, Kh, K ),
	kvo_k_memberchk_compared( Ord, Vh, KVs, K, V ).

kvo_k_memberchk_compared( <, _OldVh, OldT, K, V ) :-
    OldT = [KV|T],
    arg( 1, KV, Kh ),
    arg( 2, KV, Vh ),
    compare( Ord, Kh, K ),
    kvo_k_memberchk_compared( Ord, Vh, T, K, V ).
kvo_k_memberchk_compared( =, Vh, _T, _K, Vh ).
kvo_k_memberchk_compared( =, _OldVh, OldT, K, V ) :-
    OldT = [KV|T],
    arg( 1, KV, Kh ),
    arg( 2, KV, Vh ),
    compare( Ord, Kh, K ),
    kvo_k_memberchk_compared( Ord, Vh, T, K, V ).
