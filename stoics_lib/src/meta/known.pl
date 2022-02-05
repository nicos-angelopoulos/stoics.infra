
:- lib(suggests(pack_errors)).

:- lib(mod_goal/2).
:- lib(imported_from/2).

/** known( +Goal ).
    known( +Goal, +Cat ).
    known( +Goal, +Tkn, +Cat ).

If call(Goal) fails, then an error is thrown (via pack_errors)
saying that Tkn (usually the first arg of Goal) is not 
recognised as belonging to category Cat.

The main idea is to uniformly deal with failure when calling predicates for which the clause
definitions expect a ground 1st argument.

This meta-predicate 
  1.  provides a uniform way of dealing with failure on ground 1st argument clauses
  2.  avoids the creation of an intermediate predicate

Cat should be of the form
  * values(Cat) 
     error shows Cat as the name, and the values of the first arg of Goal as the accepted values
  * 'values()'
     values of the first Tkn arg of Goal are 
  * arbitrary_term
     in which case is taken to be the category name

If Tkn is missing is taken to be the first arg of Goal.

If Cat is missing it is taken to be values().

Goal is called deterministically (ie a cut is placed right after is called),
it can thus only succeed once.

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

?- known(theme_background(wrong,Clr), values(colour_theme)).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: colour_theme (values: [colour,monochromoe])

?- known(theme_background(wrong,Clr), ex_token, values()).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: colour_theme (values: [colour,monochromoe])
==

@author nicos angelopoulos
@version  0.1 2017/2/22
@version  0.2 2019/7/25,  Goal is now passed through mod_goal/2
@version  0.3 2019/7/25,  Allow module prepended Goal.

*/

known( G ) :-
    known( G, values() ).

known( G, Cat ) :-
    ( G = _:Goal -> true; Goal = G ),
    arg( 1, Goal, Tkn ),
    known( G, Tkn, Cat ).

known( G, _Tkn, _Cat ) :-
    mod_goal( G, MG ),
    call( MG ),
    !.
known( G, Tkn, Cat ) :-
    ( G = Gmod:Goal -> true; imported_from(G,Gmod), Goal = G ),
    functor( Goal, Gn, Ga ),
    known_not( Cat, Gmod, Goal, Gn/Ga, Tkn ),
    !.

known_not( values(), Gmod, G, Gspc, Tkn ) :-
    known_goal_values( G, Gmod, Vals ),
    term_to_atom( Vals, Vatm ),
    atom_concat( 'value in ', Vatm, ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat), Gmod:Gspc) ).
known_not( values(Cat), Gmod, G, Gspc, Tkn ) :-
    known_goal_values( G, Gmod, Vals ),
    term_to_atom( Vals, Vatm ),
    atomic_list_concat( [Cat,' (values: ',Vatm,')'], ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat),Gmod:Gspc) ).
known_not( ErrCat, Gmod, _G, Gspc, Tkn ) :-
    throw( pack_error(wrong_token(Tkn,ErrCat), Gmod:Gspc) ).

known_goal_values( G, Gmod, Vals ) :-
    functor( G, Pname, Parity ),
    functor( Head, Pname, Parity ),
    findall( Fst, (Gmod:clause(Head,_),arg(1,Head,Fst)), Vals ).
