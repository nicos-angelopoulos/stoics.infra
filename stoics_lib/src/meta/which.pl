
:- lib(mod_goal/2).
:- lib(position/3).

%% which( +Goal, +Term, -Indices ).
%
% Indices are those indexing Term elements which suceed when called on Goal.
% Works on lists and compound Terms.
%
%==
% lib( odd/1 ).
% numlist( 1, 10, OneTen ),
% which( odd, OneTen, Indices ).
%
% OneTen = [1, 2, 3, 4, 5, 6, 7, 8, 9|...],
% Indices = [1, 3, 5, 7, 9].
%
% ?- numlist( 1, 11, Eleven ), Term =.. [t|Eleven], which( odd, Term, Is ).
% Eleven = [1, 2, 3, 4, 5, 6, 7, 8, 9|...],
% Term = t(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
% Is = [1, 3, 5, 7, 9, 11].
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
% @version  0.2 2014/10/8       now uses position/3
% @see R's which()
% @tbd implement ala library(apply)
% 
which( Goal, List, Indices ) :-
    mod_goal( Goal, Moal ),
    findall( I, (position(I,List,Ith),once(call(Moal,Ith))), Indices ).
    % findall( I, (nth1(I,List,Ith),once(call(Goal,Ith))), Indices ).
