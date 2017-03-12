
:- lib(en_list/2).
:- lib(max_min_list/3).

n_breaks_defaults( Defs ) :-
	Defs = [centre(false),fixed_width(false)].

/** n_breaks( +Vector, +N, -Breaks, -Opts ).

For a vector of values, create N break points.

The number of Breaks is always odd when Centre is true. This interprets odd N as the number of 
break points, even if N it is taken to be the number of intervals.

==
?- n_breaks( [1,3,4,4,5,5,6,8], 4, Bs, [] ).
Bs = [1.0, 2.75, 4.5, 6.25, 8.0].

?- n_breaks( [0.21,3,4,4,5,5,6,8], 4, Bs, [centre(1)] ).
Bs = [0.21, 0.4075, 0.605, 0.8025, 1.0, 2.75, 4.5, 6.25, 8.0].

?- n_breaks( [0.21,3,4,4,5,5,6,8], 4, Bs, [centre(1),fixed_width(true)] ).
Bs = [-6.0, -4.25, -2.5, -0.75, 1.0, 2.75, 4.5, 6.25, 8.0].
==

Opts 
    
  * centre(Centre=false)
    when an arithmetic value is given, the breaks are symmetrically split left and right of Centre

  * fixed_width(Sym=false)
    if true and Centre arithmetic, the shorter of the left or right is extended to keep
    the breaks of fixed width

@author  nicos angelopoulos
@version 0.1 2015/5/27

@tbd add some polymorphism for Vector

*/

n_breaks( Vals, N, Breaks, ArgS ) :-
    en_list( ArgS, Args ),
    n_breaks_defaults( Defs ),
    append( Defs, Args, Opts ),
	memberchk( centre(Centre), Opts ),
	NumN is N,
	n_breaks_centre( Centre, Vals, NumN, Breaks, Opts ).

n_breaks_centre( Centre, Vals, N, Breaks, Opts ) :-
	number(Centre),
	!,
	memberchk( fixed_width(Fixed), Opts ),
	n_breaks_fixed_centre( Fixed, Centre, Vals, N, Breaks ).

n_breaks_centre( _Centre, Vals, N, Breaks, _Opts ) :-
	max_min_list( Vals, Max, Min ),
	n_breaks_range( Min, Max, N, Breaks ).

n_breaks_fixed_centre( true, Centre, Vals, N, Breaks ) :-
	max_min_list( Vals, Max, Min ),
	RelMax is max( abs(Max-Centre), abs(Centre - abs(Min)) ),
	ConcMax is Centre + RelMax,
	ConcMin is Centre - RelMax,
	Half is floor( N ),
	n_breaks_range( ConcMin, Centre, Half, Left ),
	n_breaks_range( Centre, ConcMax, Half, [_|Right] ),
	append( Left, Right, Breaks ).
n_breaks_fixed_centre( false, Centre, Vals, N, Breaks ) :-
	max_min_list( Vals, Max, Min ),
	Half is floor( N ),
	n_breaks_range( Min, Centre, Half, Left ),
	n_breaks_range( Centre, Max, Half, [_|Right] ),
	append( Left, Right, Breaks ).

n_breaks_range( Min, Max, N, Breaks ) :-
	Ivl is abs(Max-Min) / N,
	findall( Break, (between(0,N,I),Break is Min + (Ivl * I)), Breaks ).
