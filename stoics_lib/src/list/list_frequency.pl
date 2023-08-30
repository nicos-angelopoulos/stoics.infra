
:- ensure_loaded( library(terms) ).
:- lib(en_list/2).
:- lib(mod_goal/2).
:- lib(kv_transpose/2).

list_frequency_defaults( Defs ) :- 
     Defs = [ bins(false),
              order(false),
              transpose(false),
              variant(true),
              zero(false)  ].

/** list_frequency( +List, -Frequencies ).
    list_frequency( +List, -Frequencies, +Opts ).

Frequencies is a list of Term-Freq -pairs with Freq being the number of times each term (and its variants) 
appear in the List.

Opts
  * bins(Bins=false)
    if a list of values, List elements are placed in (=<) bins, if non =|false|= atom,\br
    it should be predicate name that produces a bin name for the element.<br>
    Can also be a pairlist of label-break.value elements, in which case,<br>
    the last bin should also be given with something like Bin-inf.
  * order(Ord=false)
     order of results: true sorts by element, freq sorts by frequency, and false for no sorting 
  * transpose(T=false)
     when true returns the elements of Frequencies as Freq-Term
  * variant(Var=true)
     when false compare elements with ==
  * zero(Zero=false)
     whether to include zero counter elements (Zero should be the list of expected elements)

==
?- list_frequency( [c,a,b,b,a,b,c,d], Freqs ).
Freqs = [c-2, a-2, b-3, d-1].

?- list_frequency( [c,a,a,b,a,b,c,d], Freqs, order(true) ).
Freqs = [a-3, b-2, c-2, d-1].

?- list_frequency( [c,a,b,a,b,c], Freqs, transpose(true) ).
Freqs = [2-c, 2-a, 2-b].

?- list_frequency( [c,a,b,a,b,c], Freqs, zero([b,a,c,d]) ).
Freqs = [b-2, a-2, c-2, d-0].

?- list_frequency( [a(X),b(Y),a(Z)], Freqs ).
Freqs = [a(X)-2, b(Y)-1].

?- list_frequency( [a(X),b(Y),a(Z)], Freqs, variant(false) ).
Freqs = [a(X)-1, b(Y)-1, a(Z)-1].

?- list_frequency( [a(X),b(Y),a(Z),a(X)], Freqs, variant(false) ).
Freqs = [a(X)-2, b(Y)-1, a(Z)-1].

?- list_frequency( [1,2,10,11,12,21,22], Freqs, bins([10,20]) ).
Freqs = [1-3, 2-2, 3-2].

?- list_frequency( [1,2,10,11,12,21,22], Freqs, bins([bin_1-10,bin_2-20,bin_3-inf]) ).
Freqs = [bin_1-3, bin_2-2, bin_3-2].

?- assert( (let_num(Let,Num) :- atom_codes(Let,[Code]),Num is Code-96) ).

?- list_frequency( [a,b,c,c,b,a,d], Freqs, bins(let_num) ).
Freqs = [1-2, 2-2, 3-2, 4-1].

?- list_frequency( [1,2,10,11,12,21,22], Freqs, bins(0-5) ).
Freqs = ['(0-5]'-2, '(5-10]'-1, '(10-15]'-2, '(20-25]'-2].
==

NOTE: arguments changed between 0.2 and 0.3.

@author nicos angelopoulos
@version  0.2 2015/11/25, added /3 version where wnd is Expected and examples
@version  0.3 2016/12/16, changed /3 version to 3rd being the options. added options
@version  0.4 2022/12/10, option bins(Bins)

*/


list_frequency( List, Freqs ) :-
     list_frequency( List, Freqs, [] ).

list_frequency( List, Freqs, ArgS ) :-
     en_list( ArgS, Args ),
     list_frequency_defaults( Defs ),
     append( Args, Defs, Opts ),
     options( order(Ord), Opts ),
     options( transpose(T), Opts ),
     options( variant(Vnt), Opts ),
     options( zero(Zero), Opts ),
     options( bins(Bins), Opts ),
     ( Ord == false -> Cord = false; Cord = true ),
     list_frequency_initial_counts( Zero, Cord, T, Iounts, Bero ),
     list_frequency_bin_goal( Bins, List, Gins ),
     list_frequency( List, Gins, T, Ord, Vnt, Bero, Iounts, FreqsPrv ),
     list_frequency_ord_freq( Ord, T, FreqsPrv, Freqs ).

list_frequency_ord_freq( false, _T, Prov, Freqs ) :-
     Prov = Freqs.
list_frequency_ord_freq( freq, T, Prov, Freqs ) :-
     ( T == true -> 
          sort( 1, @>=, Prov, Freqs )
          ;
          kv_transpose( Prov, Trans ),
          sort( 1, @>=, Trans, Sort ),
          kv_transpose( Sort, Freqs )
     ).
list_frequency_ord_freq( true, _T, Prov, Freqs ) :-
     Prov = Freqs.
/*
 
*/
list_frequency( [], _Bins, _T, _Ord, _Vnt, _Bero, Freqs, Freqs ).
list_frequency( [H|Tail], Bins, T, Ord, Vnt, Bero, Iounts, Freqs ) :-
     list_frequency_bin( Bins, H, Bin ),
     % list_frequency_elem( Ord, T, Vnt, Bero, H, Iounts, Nounts ),
     list_frequency_elem( Ord, T, Vnt, Bero, Bin, Iounts, Nounts ),
     list_frequency( Tail, Bins, T, Ord, Vnt, Bero, Nounts, Freqs ).

list_frequency_bin( false, H, Bin ) :-
     !,
     Bin = H.
list_frequency_bin( [B|Ns], H, Bin ) :-
     !,
     list_frequency_nth_bin( [B|Ns], H, 1, Bin ).
list_frequency_bin( Goal, H, Bin ) :-
     call( Goal, H, Bin ).

list_frequency_nth_bin( [], _H, I, Bin ) :-
     Bin is I.
list_frequency_nth_bin( [Bin-Coff|_Ns], H, _I, OBin ) :-
     H =< Coff,
     !,
     OBin = Bin.
list_frequency_nth_bin( [B|_Ns], H, I, Bin ) :-
     ( Bin-Coff = B -> 
          true
          ;
          Coff = B
     ),
     H =< Coff,
     !,
     Bin is I.
list_frequency_nth_bin( [_B|Ns], H, I, Bin ) :-
     J is I + 1,
     list_frequency_nth_bin( Ns, H, J, Bin ).

list_frequency_elem( true, T, Vnt, Bero, H, Iounts, Nounts ) :-
     list_frequency_elem_ord( Iounts, H, T, Vnt, Bero, Nounts ).
list_frequency_elem( false, T, Vnt, Bero, H, Iounts, Nounts ) :-
     list_frequency_elem_nat( Iounts, H, T, Vnt, Bero, Nounts ).

list_frequency_elem_ord( [], Elem, T, _Vnt, Bero, Counts ) :- 
     ( Bero == false -> list_frequency_pair( T, Elem, 1, Pair ), Counts = [Pair] ; Counts = [] ).
list_frequency_elem_ord( [H|Tail], Elem, T, Vnt, Bero, Counts ) :- 
     list_frequency_de_pair( T, H, Kh, Vh ),
     list_frequency_ord_compare( Vnt, Kh, Elem, Op ),
     list_frequency_elem_ord_op( Op, H, Kh, Vh, Tail, Elem, T, Vnt, Bero, Counts ).

list_frequency_elem_ord_op( =, _H, Kh, Vh, Tail, _Elem, T, _Vnt, _Bero, Counts ) :-
     list_frequency_pair_increase( T, Kh, Vh, Pair ),
     Counts = [Pair|Tail].
list_frequency_elem_ord_op( <, H, _Kh, _Vh, Tail, Elem, T, Vnt, Bero, Counts ) :-
     Counts = [H|TCounts],
     list_frequency_elem_ord( Tail, Elem, T, Vnt, Bero, TCounts ).
list_frequency_elem_ord_op( >, H, _Kh, _Vh, Tail, Elem, T, _Vnt, Bero, Counts ) :-
     ( Bero == false -> list_frequency_pair( T, Elem, 1, Pair ), Counts = [Pair,H|Tail]; Counts = [H|Tail] ).

list_frequency_elem_nat( [], Elem, T, _Vnt, Bero, Counts ) :-
     ( Bero == false -> list_frequency_pair( T, Elem, 1, Pair ), Counts = [Pair] ; Counts = [] ).
list_frequency_elem_nat( [H|Tail], Elem, T, Vnt, Bero, Counts ) :-
     list_frequency_de_pair( T, H, Kh, Vh ),
     list_frequency_nat_eq( Vnt, Kh, Elem, Eq ),
     list_frequency_elem_nat_eq( Eq, H, Kh, Vh, Tail, Elem, T, Vnt, Bero, Counts ).
          
list_frequency_elem_nat_eq( =, _H, Kh, Vh, Tail, _Elem, T, _Vnt, _Bero, Counts ) :-
     list_frequency_pair_increase( T, Kh, Vh, Pair ),
     Counts = [Pair|Tail].
list_frequency_elem_nat_eq( <>, H, _Kh, _Vh, Tail, Elem, T, Vnt, Bero, Counts ) :-
     Counts = [H|Tounts],
     list_frequency_elem_nat( Tail, Elem, T, Vnt, Bero, Tounts ).

list_frequency_ord_compare( true, H, Elem, Op ) :-
     ( variant(H,Elem) -> Op = (=) ; compare( Op, H, Elem ) ).

list_frequency_nat_eq( true, Seen, Elem, Eq ) :-
     ( variant(Seen,Elem) -> Eq = (=) ; Eq = (<>) ).
list_frequency_nat_eq( false, Seen, Elem, Eq ) :-
     ( Seen == Elem -> Eq = (=); Eq = (<>) ).
     
list_frequency_pair_increase( true, Kh, Vh, Vi-Kh ) :-
     Vi is Vh + 1.
list_frequency_pair_increase( false, Kh, Vh, Kh-Vi ) :-
     Vi is Vh + 1.

list_frequency_de_pair( true, Times-Elem, Elem, Times ).
list_frequency_de_pair( false, Elem-Times, Elem, Times ).

list_frequency_pair( true, Elem, Times, Times-Elem ).
list_frequency_pair( false, Elem, Times, Elem-Times ).

list_frequency_initial_counts( false, _Ord, _T, [], false ) :- !.
list_frequency_initial_counts( List, Ord, T, Counts, true ) :-
     is_list( List ), ground( List ),
     findall( KV,  (member(Elem,List), list_frequency_pair(T,Elem,0,KV)), CountsPrv ),
     list_freqency_order( Ord, T, CountsPrv, Counts ).

list_freqency_order( false, _T, Freqs, Freqs ).
list_freqency_order( true, T, CountsPrv, Counts ) :- 
     list_frequency_order_elem( T, CountsPrv, Counts ).
list_freqency_order( freq, T, CountsPrv, Counts ) :-
     list_frequency_order_freq( T, CountsPrv, Counts ).

list_frequency_order_elem( true, Prv, Freqs ) :-
     kv_transpose( Prv, ElemFreqPrs ),
     sort( ElemFreqPrs, TFreqs ),
     kv_transpose( TFreqs, Freqs ).
list_frequency_order_elem( false, Prv, Freqs ) :-
     sort( Prv, Freqs ).

list_frequency_order_freq( true, Prv, Freqs ) :-
     sort( Prv, Freqs ).
list_frequency_order_freq( false, Prv, Freqs ) :-
     kv_transpose( Prv, FreqElemPrs ),
     sort( FreqElemPrs, TFreqs ),
     kv_transpose( TFreqs, Freqs ).

list_frequency_bin_goal( false, _List, Moal ) :-
     !,
     Moal = false.
list_frequency_bin_goal( [B|Ns], _List, Gins ) :-
     !,
     Gins = [B|Ns].
list_frequency_bin_goal( Start-Step, List, Gins ) :-
     !,
     max_list( List, Max ),
     % First = Start + Step,
     Start =< Max,
     list_frequency_bin_breaks( Start, Max, Step, Gins ).
list_frequency_bin_goal( Goal, _List, Moal ) :-
    mod_goal( Goal, Moal ).

list_frequency_bin_breaks( Curr, Max, Step, Gins ) :-
     Cutoff is Curr + Step,
     Curr > Max,
     !,
     atomic_list_concat( ['(',Curr,'-',Cutoff,']'], '',  Bin ),
     Gins = [Bin-Cutoff].
list_frequency_bin_breaks( Curr, Max, Step, Gins ) :-
     Cutoff is Curr + Step,
     atomic_list_concat(['(',Curr,'-',Cutoff,']'], '',  Bin ),
     Gins = [Bin-Cutoff|Tins],
     list_frequency_bin_breaks( Cutoff, Max, Step, Tins ).
% list_frequency_bin_breaks( Curr, Max, Step, Gins ) :-
     % list_frequency_bin_breaks( Cutoff, Max, Step, Tins ).


/*
list_frequency( [H|T], [H-HTimes|CountedT] ) :-
        list_frequency_1( T, H, 1, HTimes, ReducedT ),
        list_frequency( ReducedT, CountedT ).
list_frequency( [], [] ).

list_frequency_1( [H|T], El, Acc, Count, RedT ) :-
        variant( El, H ),
        !,
        Acc1 is Acc + 1,
        list_frequency_1( T, El, Acc1, Count, RedT ).
list_frequency_1( [H|T], El, Acc, Count, [H|RedT] ) :-
        list_frequency_1( T, El, Acc, Count, RedT ).
list_frequency_1( [], _El, Acc, Acc, [] ).

list_frequency( List, Expct, Freqs ) :-
     findall( Elem-0, member(Elem,Expct), Counters ),
     list_frequency_expected( List, Counters, Freqs ).

list_frequency_expected( [], Freqs, Freqs ).
list_frequency_expected( [H|T], Counters, Freqs ) :-
     list_frequency_expected_increase_counter( Counters, H, Next ),
     list_frequency_expected( T, Next, Freqs ).

list_frequency_expected_increase_counter( [], Elem, _Next ) :-
     throw( no_counter_for(Elem) ).
list_frequency_expected_increase_counter( [Elem-Curr|T], Elem, [Elem-Next|T] ) :-
     !,
     Next is Curr + 1.
list_frequency_expected_increase_counter( [H|T], Elem, [H|Next] ) :-
     list_frequency_expected_increase_counter( T, Elem, Next ).
     */
