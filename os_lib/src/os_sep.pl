
:- lib( stoics_lib:en_list/2 ).

/** os_sep( -Sep ).
    os_sep( -Sep, Opts ).

Read the default or Opts provided separator.

Opts 
  * separator(Sep=Sep)    
    expansion of sep(Sep) - canonical is sep(Sep)
 
  * sep(Sep='_')
    shortened separator(). Sep is the separator for stem-file parts

==
?- os_sep( Sep ).
Sep = '_'.

?- os_sep( Sep, true ).
Sep = '_'.

?- os_sep( Sep, separator(x) ).
Sep = x.

?- os_sep( Sep, [separator(x),sep(y)] ).
Sep = y.

==

*/
os_sep( Sep ) :-
	os_sep( Sep, [] ).

os_sep( Sep, OptS ) :-
	en_list( OptS, Opts ),
	os_sep_opts( Sep, Opts ).

os_sep_opts( Sep, Opts ) :-
	memberchk( sep(Sep), Opts ),
	!.
os_sep_opts( Sep, Opts ) :-
	memberchk( separator(Sep), Opts ),
	!.
os_sep_opts( Sep, _Opts ) :-
	current_prolog_flag( filename_separator, Sep ),
	!.
os_sep_opts( '_', _Opts ).
