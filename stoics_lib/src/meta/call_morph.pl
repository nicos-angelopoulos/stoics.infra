
:- lib(stoics_lib:mod_goal/4).

/**  call_morph( +Term, +Input, -Morphed, +Opts ).

If Term/+2 is callable, then it is called on Input and Morphed;
else Morphed is unified to Term. The latter is also the case, if
the call fails.

Opts are passed to mod_call/4

The main perceived use case is for the ability to use options that
either transform another option or pass a static value. 
For instance to create output file stems from input filenames.

==
?- use_module(library(lib)).
?- lib(os_lib).
?- assert( (to_stem(File,Stem) :- os_ext(Ext,Stem,File)) ).
?- call_morph( to_stem, input.txt, Stem, true ).
Stem = input.
==

@author nicos angelopoulos
@version  0.1 2019/2/22

*/
call_morph( Term, Input, Morphed, Opts ) :-
    mod_goal( user, Term, Goal, Opts ),
    callable_morph( Goal, Input, Term, Morphed ).

callable_morph( Goal, Input, _Term, Morphed ) :-
    catch( call(Goal,Input,Morphed), _, fail ),
    !.
callable_morph( _Goal, _Input, Term, Morphed ) :-
    Morphed = Term.
