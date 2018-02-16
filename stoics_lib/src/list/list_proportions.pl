
list_proportions_defaults( [min_max(false),to_range(false),on_call(false)] ).

/** list_proportions( +List, -Propos, +Opts ).

Create the proportions of each element within List.<br>
Where the proportion of an element it is 0-1 location within the range<br>

Opts
  * min_max(MinMax=false)
     else give a range (r(Min,Max)) that are assumed to be the min and values of list <br>
     so the code doesn't have to calculate them
  * to_range(ToR=false)
     else give a range (r(ToMin,ToMax)) to which to cast the proportions

==
?- list_proportions( [1,2,3,4], Props ).
Props = [0, 0.3333333333333333, 0.6666666666666666, 1].

?- list_proportions( [1,2,3,4], Props, to_range(r(2,8)) ).
Props = [2, 4.0, 6.0, 8].

?- list_pro
==

@author nicos angelopoulos
@version  0.1 2018/2/16

*/
list_proportions( List, Props ) :-
    list_proportions( List, Props, [] ).

list_proportions( List, Props, Args ) :-
    options_append( list_proportions, Args, Opts ),
    ( memberchk(min_max(r(Rmin,Rmax)),Opts) ->
        true
        ;
        min_max( List, Rmin, Rmax )
    ),
    ( memberchk(to_range(r(Tmin,Tmax)),Opts) ->
        Tfactor is Tmax - Tmin
        ;
        Tfactor is 1, Tmin is 0
    ),
    options( on_call(OnG), Opts ),
    Range is Rmax - Rmin,
    list_proportions( List, Rmin, Range, Tmin, Tfactor, Props ).

list_proportions( [], _Min, _Range, _Tmin, _Tf, [] ).
list_proportions( [H|T], Min, Range, Tmin, Tf, [Hp|TProps] ) :-
    Hp is Tmin + ((( H - Min ) / Range ) * Tf),
    list_proportions( T, Min, Range, Tmin, Tf, TProps ).
