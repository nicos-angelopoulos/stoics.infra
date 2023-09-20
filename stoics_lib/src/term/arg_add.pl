/** arg_add( ?N, +Term, +ArgS, -New ).

Add an Arg or list of Args to the Nth position of Term, resulting in New.

N is an arithmetic expression (v.2). N can be a variable (v.3) in which case the
length + 1 is returned and ArgS are appended at end. 

ArgS can be a list of args (v.3).

==
?- arg_add( 2, x(4,3,1), 2, X ).
X = x(4, 2, 3, 1).

?- arg_add( L, x(1,2,3), [4,5], Five ).
Five = x(1, 2, 3, 4, 5).

==

@author nicos angelopoulos
@version  0.1 2016/10/28
@version  0.2 2018/12/4,  add expressions for N, added to stoics_lib
@version  0.3 2022/12/21, Arg can be a list now and Nin a var. examples.

*/
arg_add( Nin, Term, Arg, New ) :-
    Term =.. [Name|Args],
    ( var(Nin) -> 
          length( Args, M ),
          N is M + 1
          ;
          N is Nin
    ),
    M is N - 1,
    length( Prefix, M ),
    once( append(Prefix,Postfix,Args) ),
    ( is_list(Arg) -> append(Arg,Postfix,Tail); Tail = [Arg|Postfix] ),
    append( Prefix, Tail, Nrgs ),
    New =.. [Name|Nrgs].
