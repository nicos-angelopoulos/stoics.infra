
:- use_module(library(lists)).  % append/3, is_list/1, memberchk/2.

:- lib(imported_from/2).

:- lib(suggests(pack_errors)).

mod_goal_defaults( [override(false)] ).

/** mod_goal( +Mod, +Goal, -Moal, +Opts ).
    mod_goal( +Mod, +Goal, -Moal ).
    mod_goal( -Mod, -Goal, +Moal ).
    mod_goal( +Goal, -Moal ).

Construct and deconstruct a goal and its module prepended form.
When Mod is missing imported_from/2 is used to locate the module. 
If the latter fails, the module is set to _user_. Opts are passed to 
errors so real source can be reported.

Opts
  * override(OverR=false)
    what to do when constructing over a Goal that already has a module prepention
    * false
      ignores the new Mod
    * true
      replaces Goal's prepention with Mod 
    * error
      reports the conflict

When de-constructing, Goal will be a goal with no module prepent. 
When constructing, Moal will be a module prepented goal

Incompatibility: 0.3 removed the mod_goal/4 version that had OverR as 3rd argument.

As of 0.4 imported_from/2 is used to find default module.

==
?- mod_goal( mod1, g1, MG ).
MG = mod1:g1.


?- mod_goal( M, G, mod2:g2(a,b,c) ).
M = mod2,
G = g2(a, b, c).

?- mod_goal( M, G, MG ).
ERROR: auxil:mod_goal/3: Ground argument expected either at: [1,2], or at: 3 

?- mod_goal( m, k:g(a), MG ).
MG = k:g(a).

?- mod_goal( m, k:g(a), true, MG ).
MG = m:g(a).

?- mod_goal( g(a), MG ).
MG = user:g(a).

?- mod_goal( user, foo:bar(x), Moal, [override(false)] ).
Moal = foo:bar(x).

?- mod_goal( user, foo:bar(x), Moal, [override(true)] ).
Moal = user:bar(x).

?- mod_goal( user, foo:bar(x), Moal, [override(error)] ).
ERROR: stoics_lib:mod_goal/3: Module to fix-on: user differs from module attached in: foo:bar(x)
?- mod_goal( user, foo:bar(x), Moal, [override(error),caller:id/3] ).
ERROR: stoics_lib:mod_goal/3: Module to fix-on: user differs from module attached in: foo:bar(x)
ERROR: Trail: [caller:id/3]
==

@author  nicos angelopoulos
@version 0.1   2014
@version 0.2   2017/9/25,  default value for Override changed to false, added mod_goal/2
@version 0.3   2018/10/11, update error + options version, pushes trails to errorrs
@version 0.4   2022/02/05, use imported_from/2 for locating default Mod
@version 0.5   2022/11/19, when imported_from/2 fails, try predicate_property/2, currently with visible

*/
mod_goal( Goal, Moal ) :-
     \+ var(Goal),
     Goal = _:_G,
     !,
     Moal = Goal.
mod_goal( Goal, Moal ) :-
     imported_from(Goal,Gmod),
     !,
     mod_goal( Gmod, Goal, Moal, [] ).
mod_goal( Goal, Moal ) :-
     predicate_property( Gmod:Goal, visible ),
     !,
     mod_goal( Gmod, Goal, Moal, [] ).
mod_goal( Goal, Moal ) :-
    functor( Goal, Pname, _Art ),
    current_predicate( Pname/_ ),  % fixme, add opt for +args (used in meta calls)
    !,
    mod_goal( user, Goal, Moal, [] ).
mod_goal( Goal, _Moal ) :-
     Fmt = 'Can not locate definition for goal: ~w',
     message_report( Fmt, [Goal], error ),
     fail.

mod_goal( Mod, Goal, Moal ) :-
    mod_goal( Mod, Goal, Moal, [] ).
mod_goal( Mod, Goal, Moal, Args ) :-
    mod_goal_defaults( Defs ),
    ( is_list(Args) -> append(Args,Defs,Opts); append([Args],Defs,Opts) ),
    mod_goal_opts( Mod, Goal, Moal, Opts ).

mod_goal_opts( Mod, Goal, Moal, Opts ) :-
    ground( Mod ),
    \+ var( Goal ),
    !,
    memberchk( override(OveR), Opts ),
    mod_goal_gen( Mod, Goal, OveR, Moal, Opts ).
mod_goal_opts( Mod, Goal, Moal, _Opts ) :-
    \+ var( Moal ),
    !,
    ( Moal = Mod:Goal -> true; Mod=user, Goal=Moal ).
mod_goal_opts( Mod, Goal, Moal, Opts ) :-
    Self = stoics_lib:mod_goal/3,
    throw( pack_error(arg_ground_in_one_of([1+2,3],[Mod+Goal,Moal]),[Self|Opts]) ).
mod_goal_opts( Mod, Goal, Moal, Opts ) :-
    memberchk( override(Over), Opts ),
    ground( Mod ),
    ground( Goal ),
    ground( Over ),
    !,
    mod_goal_gen( Mod, Goal, Over, Moal, Opts ).
mod_goal_opts( Mod, Goal, Over, _Moal ) :-
    throw( pack_error(auxil,mod_goal/4,ground([1,2,3],[Mod,Goal,Over])) ).

mod_goal_gen( Mod, Goal, Over, Moal, Opts ) :-
    Goal = Mod1:Goal1,
    !,
    mod_goal_over( Over, Mod, Mod1, Goal1, Moal, Opts ).
mod_goal_gen( Mod, Goal, _Over, Moal, _Opts ) :-
    Moal = Mod:Goal.

mod_goal_over( false, _Mod, Mod1, Goal1, Moal, _Opts ) :-
    Moal = Mod1:Goal1.
mod_goal_over( true, Mod, _Mod1, Goal1, Moal, _Opts ) :-
    Moal = Mod:Goal1.
mod_goal_over( error, ModIn, ModAttached, Naked, _Moal, Opts ) :-
    % throw( pack_error(stoics_lib,mod_goal/4,modules_clash(ModIn,ModAttached,Naked)) ).
    Epts = [stoics_lib:mod_goal/3|Opts],
    throw( pack_error(modules_clash(ModIn,ModAttached,Naked),Epts) ).

:- multifile( pack_errors:message/3 ).

pack_errors:message( modules_clash(In,Attached,Goal) ) -->
    ['Module to fix-on: ~w differs from module attached in: ~w'- [In,Attached:Goal] ].
