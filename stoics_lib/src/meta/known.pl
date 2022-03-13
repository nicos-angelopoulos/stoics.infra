
:- lib(suggests(pack_errors)).

:- lib(mod_goal/2).
:- lib(imported_from/2).

:- dynamic(known_call_succ/1).
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

?- lib(os_lib).

?- cd(pack('stoics_lib/src/meta')).

?- os_file( Os ).
Os = call_morph.pl ;
...

?- os_file( Os, solutions(all) ).
ERROR: os_lib:os_lib:os_file_sol/7: Token: all, is not a recognisable: value in [single,findall]
==

@author nicos angelopoulos
@version  0.1 2017/2/22
@version  0.2 2019/7/25,  Goal is now passed through mod_goal/2
@version  0.3 2022/2/13,  Allow module prepended Goal. Allow multi solution Goals (see os_file examples).

*/

known( G ) :-
    known( G, values() ).

known( G, Cat ) :-
    ( G = _:Goal -> true; Goal = G ),
    arg( 1, Goal, Tkn ),
    known( G, Tkn, Cat ).

known( G, Tkn, Cat ) :-
    mod_goal( G, MG ),
    MG = Mod:Goal,
    functor( Goal, Pn, Pa ),  % stoics_lib:use arity/3 if you get problems with non () functor calls
    MGi = Mod:Pn/Pa,
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
known_not( values(), Vals, Gmod, _G, Gspc, Tkn ) :-
    term_to_atom( Vals, Vatm ),
    atom_concat( 'value in ', Vatm, ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat), Gmod:Gspc) ).
known_not( values(Cat), Vals, Gmod, _G, Gspc, Tkn ) :-
    term_to_atom( Vals, Vatm ),
    atomic_list_concat( [Cat,' (values: ',Vatm,')'], ErrCat ),
    throw( pack_error(wrong_token(Tkn,ErrCat),Gmod:Gspc) ).
known_not( ErrCat, Gmod, _G, Gspc, Tkn ) :-
    throw( pack_error(wrong_token(Tkn,ErrCat), Gmod:Gspc) ).

known_goal_values( G, Gmod, Vals ) :-
    functor( G, Pname, Parity ),
    functor( Head, Pname, Parity ),
    findall( Fst, (Gmod:clause(Head,_),arg(1,Head,Fst)), Vals ).
