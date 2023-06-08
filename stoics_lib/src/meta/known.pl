
:- lib(suggests(pack_errors)).

:- lib(mod_goal/2).

:- dynamic(known_call_succ/1).

known_defaults( Args, Defs ) :-
     memberchk( arg1(Arg1), Args ),
     Defs = [  category(values()),
               solutions(first),
               token(Arg1),
               options_types([solutions-oneof([all,first])])
     ].

/** known(+Goal).
    known(+Goal, +Opts).

If call(Goal) fails, then an error is thrown (via pack_errors)
saying that Tkn (usually the first arg of Goal) is not 
recognised as belonging to category Cat.

The main idea is to uniformly deal with failure when calling predicates for which the clause
definitions expect a ground 1st argument.

This meta-predicate 
  1.  provides a uniform way of dealing with failure on ground 1st argument clauses
  2.  avoids the creation of an intermediate predicate

Opts
  * category(Cat=values())
    Cat should be of the form
    * values(Cat) 
       error shows Cat as the name, and the values of the first arg of Goal as the accepted values
    * 'values()'
      values of the first Tkn arg of Goal are 
    * arbitrary_term
      in which case is taken to be the category name
  * solutions(first)
    set to =|all|= for backtracking
  * token(Tkn)
    defaults to the first argument of Goal

Goal used to be called deterministically, version 0.3 made this non-det and 0.4 added an option
to control this.

==
?- [user].
theme_background(colour, blue).
theme_background(monochrome, grey).
^D
?- known(theme_background(colour,Clr)).
Clr = blue.

?- known(theme_background(wrong,Clr)).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: value in [colour,monochromoe]

?- known(theme_background(wrong,Clr), colour_theme).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: colour_theme

?- known(theme_background(wrong,Clr), category(values(colour_theme))).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: colour_theme (values: [colour,monochrome])

?- known(theme_background(wrong,Clr), token(ex_token) ).
ERROR: user:theme_background/2: Token: ex_token, is not a recognisable: value in [colour,monochrome]
==

@author nicos angelopoulos
@version  0.1 2017/2/22
@version  0.2 2019/7/25,  Goal is now passed through mod_goal/2
@version  0.3 2022/2/13,  Allow module prepended Goal. Allow multi solution Goals (see os_file examples).
@version  0.4 2023/1/02,  interface change to /2 with options and solutions controlled via solutions(Sol)

*/

known( G ) :-
    known( G, [] ).

known( G, Args ) :-
    ( G = _:Goal -> true; Goal = G ),
    arg( 1, Goal, Arg1 ),
    Self = known,
    options_append( Self, Args, Opts, extra_arg(arg1(Arg1)) ),
    options( token(Tkn), Opts ),
    options( category(Cat), Opts ),
    options( solutions(Sol), Opts ),
    mod_goal( G, MG ),
    MG = Mod:Goal,
    functor( Goal, Pn, Pa ),  % stoics_lib:use arity/3 if you get problems with non () functor calls
    MGi = Mod:Pn/Pa,
    retractall( known_call_succ(MGi) ),
    known( Sol, MG, Tkn, Cat, MGi ).

known( first, MG, _Tkn, _Cat, _MGi ) :-
     call( MG ),
     !.
known( first, MG, Tkn, Cat, MGi ) :-
    MG = Gmod:Goal,
    known_goal_values( Goal, Gmod, Vals ),
    known_not( Cat, Vals, Gmod, Goal, MGi, Tkn ),
    !.
known( all, MG, Tkn, Cat, MGi ) :-
     known( MG, Tkn, Cat, MGi ).

known( MG, _Tkn, _Cat, MGi ) :-
    call( MG ),
    assert( known_call_succ(MGi) ).
known( _MG, _Tkn, _Cat, MGi ) :-
     known_call_succ( MGi ), 
     !,
     retractall( known_call_succ(MGi) ),
     fail.
known( MG, Tkn, Cat, MGi ) :-
    MG = Gmod:Goal,
    known_goal_values( Goal, Gmod, Vals ),
    known_not( Cat, Vals, Gmod, Goal, MGi, Tkn ),
    !.

known_not( _, Vals, _Gmod, _G, _Gspc, Tkn ) :-
    memberchk( Tkn, Vals ),
    !,
    fail.
known_not( values(), Vals, _Gmod, _G, Gspc, Tkn ) :-
    term_to_atom( Vals, Vatm ),
    atom_concat( 'value in ', Vatm, ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat), Gspc ) ).
known_not( values(Cat), Vals, _Gmod, _G, Gspc, Tkn ) :-
    term_to_atom( Vals, Vatm ),
    atomic_list_concat( [Cat,' (values: ',Vatm,')'], ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat),Gspc) ).
known_not( ErrCat, _Vals, _Gmod, _G, Gspc, Tkn ) :-
    % fixme: can incorporate Vals to message:
    throw( pack_error(wrong_token(Tkn,ErrCat), Gspc) ).

known_goal_values( G, Gmod, Vals ) :-
    functor( G, Pname, Parity ),
    functor( Head, Pname, Parity ),
    findall( Fst, (Gmod:clause(Head,_),arg(1,Head,Fst)), Vals ).
