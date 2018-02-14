/** skim( +Nested, -Scum, -Remains ).

	Skim the first elements (Scum) from a Nested list with the tails being the Remains.<br>
    Fails if Nested has no more elements to skim at all positions (typically a list of empty lists).

==
?- Nest = [[a,b,c],[1,2,3]], skim( Nest, Sc, Rest ).
Nest = [[a, b, c], [1, 2, 3]],
Sc = [a, 1],
Rest = [[b, c], [2, 3]].

?- Nest = [[a,b,c],[1,2,3]], skim(Nest,Sc1,Rest1), skim(Rest1,Sc2,Rest2), skim(Rest2,Sc3,Rest3).
Nest = [[a, b, c], [1, 2, 3]],
Sc1 = [a, 1],
Rest1 = [[b, c], [2, 3]],
Sc2 = [b, 2],
Rest2 = [[c], [3]],
Sc3 = [c, 3],
Rest3 = [[], []].

?- Nest = [[a,b,c],[1,2,3]], skim(Nest,Sc1,Rest1), skim(Rest1,Sc2,Rest2), skim(Rest2,Sc3,Rest3), skim(Rest3,Sc4,Rest4).
false.

==
*/
skim([], [], [] ).
skim([[H|T]|M], [H|RScum], [T|R] ) :-
     skim(M, RScum, R).
