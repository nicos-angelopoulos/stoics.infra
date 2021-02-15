
:- lib(suggests(pack_errors)).

:- lib(mod_goal/2).
:- lib(imported_from/2).

/** known( +Goal ).
    known( +Goal, +Cat ).
    known( +Goal, +Tkn, +Cat ).

    If call(Goal) fails, then an error is thrown (via pack_errors)
    saying that Tkn (usually the first arg of Goal) is not 
    recognised as belonging to category Cat.

    The idea is that Goal is a predicate whose 1st argument
    indexes a number of options and this wrapper provides 

      1.  a uniform way of dealing with failure
      2.  a way to avoid creating an intermediate predicate

    Cat should either be atomic (a description of the category
    expected for Tkn) or of the form

      * values(Cat) values of the first Tkn arg of Goal are appended to Cat
      * valuess()   values of the first Tkn arg of Coal become Cat

    If Tkn is missing is taken to be the first arg of Goal.
    If category is missing it is taken to be values().

    Goal is called deterministically (ie a cut is placed right after is called),
    it can thus only succeed once.

==
?- [user].
theme_background( colour, blue ).
theme_background( monochrome, grey ).
^D
?- known( theme_background(colour,Clr) ).
Clr = blue.

?- known( theme_background(wrong,Clr) ).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: value in [colour,monochromoe]

?- known( theme_background(wrong,Clr), colour_theme ).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: colour_theme

?- known( theme_background(wrong,Clr), values(colour_theme) ).
ERROR: user:theme_background/2: Token: wrong, is not a recognisable: colour_theme (values: [colour,monochromoe])
==

@author nicos angelopoulos
@version  0.1 2017/2/22
@version  0.1 2017/7/25,  Goal is now passed through mod_goal/2

*/

known( G ) :-
    known( G, values() ).

known( G, Cat ) :-
    arg( 1, G, Tkn ),
    known( G, Tkn, Cat ).

known( G, _Tkn, _Cat ) :-
    mod_goal( G, MG ),
    call( MG ),
    !.
known( G, Tkn, Cat ) :-
    functor( G, Gn, Ga ),
    imported_from( G, Gmod ),
    known_not( Cat, Gmod, G, Gn/Ga, Tkn ),
    !.

known_not( values(), Pack, G, Gspc, Tkn ) :-
    known_goal_values( G, Vals ),
    term_to_atom( Vals, Vatm ),
    atom_concat( 'value in ', Vatm, ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat), Pack:Gspc) ).
known_not( values(Cat), Pack, G, Gspc, Tkn ) :-
    known_goal_values( G, Vals ),
    term_to_atom( Vals, Vatm ),
    atomic_list_concat( [Cat,' (values: ',Vatm,')'], ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat),Pack:Gspc) ).
known_not( ErrCat, Pack, _G, Gspc, Tkn ) :-
    throw( pack_error(wrong_token(Tkn,ErrCat), Pack:Gspc) ).

known_goal_values( G, Vals ) :-
    imported_from( G, Gmod ),
    functor( G, Pname, Parity ),
    functor( Head, Pname, Parity ),
    findall( Fst, (Gmod:clause(Head,_),arg(1,Head,Fst)), Vals ).
