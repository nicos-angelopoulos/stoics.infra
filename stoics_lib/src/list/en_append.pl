:- lib(en_list/2).

/** en_append( +ListOr1, +ListOr2, -List ).

Enlists (en_list/2) ListOr1 and 2 before appending them. 

==
άμπελος;src/term% lib stoics_lib
%  /home/na11/.rcpl compiled 0.00 sec, 8 clauses
?- en_append( a, b, C ).
C = [a, b].

?- en_append( a, [b], C ).
C = [a, b].
==

@author nicos angelopoulos
@version  0.1 2017/01/03

*/
en_append( AlistOr, BlistOr, Clist ) :-
    en_list( AlistOr, Alist ),
    en_list( BlistOr, Blist ),
    append( Alist, Blist, Clist ).
