
:- lib(pack_errors).

/** mod_goal( +Mod, +Goal, +Override, -Moal ).
    mod_goal( +Mod, +Goal, -Moal ).
    mod_goal( -Mod, -Goal, +Moal ).

Construct and deconstruct a goal and its module prepended form.
Argumen Override, controls what happends when constructing over a Goal
that already has a module prepention: false ignores the new Mod,
true (default) replaces Goal's prepention with Mod and error
reports the conflict.

When de-constructing Goal will be a goal with no module prepent. 
When constructing Moal will be a module prepented goal

==
?- mod_goal( mod1, g1, MG ).
MG = mod1:g1.

?- mod_goal( M, G, mod2:g2(a,b,c) ).
M = mod2,
G = g2(a, b, c).

?- mod_goal( M, G, MG ).
ERROR: auxil:mod_goal/3: Ground argument expected either at: [1,2], or at: 3 

?- mod_goal( m, k:g(a), MG ).
MG = m:g(a).

?- mod_goal( m, k:g(a), false, MG ).
MG = k:g(a).
==

@author  nicos angelopoulos
@version 0.1   2014

*/
mod_goal( Mod, Goal, Moal ) :-
	ground( Mod ),
	ground( Goal ),
	!,
	mod_goal_gen( Mod, Goal, true, Moal ).
mod_goal( Mod, Goal, Moal ) :-
	ground( Moal ),
	!,
	( Moal = Mod:Goal -> true; Mod=user, Goal=Moal ).
mod_goal( _Mod, _Goal, _Moal ) :-
	throw( pack_error(auxil,mod_goal/3,ground_dual_mode([1,2],3)) ).
mod_goal( Mod, Goal, Over, Moal ) :-
	ground( Mod ),
	ground( Goal ),
	ground( Over ),
	!,
	mod_goal_gen( Mod, Goal, Over, Moal ).
mod_goal( Mod, Goal, Over, _Moal ) :-
	throw( pack_error(auxil,mod_goal/4,ground([1,2,3],[Mod,Goal,Over])) ).

mod_goal_gen( Mod, Goal, Over, Moal ) :-
	Goal = Mod1:Goal1,
	!,
	mod_goal_over( Over, Mod, Mod1, Goal1, Moal ).
mod_goal_gen( Mod, Goal, _Over, Moal ) :-
	Moal = Mod:Goal.

mod_goal_over( false, _Mod, Mod1, Goal1, Moal ) :-
	Moal = Mod1:Goal1.
mod_goal_over( true, Mod, _Mod1, Goal1, Moal ) :-
	Moal = Mod:Goal1.
mod_goal_over( error, ModIn, ModAttached, Naked, _Moal ) :-
	% throw( pack_error(stoics_lib,mod_goal/4,modules_clash(ModIn,ModAttached,Naked)) ).
	throw( pack_error(stoics_lib,modules_clash(ModIn,ModAttached,Naked)) ).

:- multifile( pack_errors:message/3 ).

pack_errors:message( modules_clash(In,Attached,Goal) ) -->
	['Module to fix-on: ~w differs from module attached in: ~w'- [In,Attached:Goal] ].
