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

?- arg_add( 3, a(a,b,d,e), c, New ).
New = a(a, b, c, d, e).

?- arg_add( -3, a(a,b,d,e), c, New ).
New = a(a, b, c, d, e).
==

@author nicos angelopoulos
@version  0.1 2016/10/28
@version  0.2 2018/12/4,  add expressions for N, added to stoics_lib
@version  0.3 2022/12/21, Arg can be a list now and N a variable. added examples
@version  0.4 2023/09/20, N can be a negative integer

*/
arg_add( Nin, Term, Arg, New ) :-
    Term =.. [Name|Args],
    ( var(Nin) -> 
          length( Args, M ),
          N is M + 1
          ;
          ( Nin < 0 ->
               functor( Term, _, TermArity ),
               N is TermArity + Nin + 2
               ;
               N is Nin
          )
    ),
    M is N - 1,
    length( Prefix, M ),
    once( append(Prefix,Postfix,Args) ),
    ( is_list(Arg) -> append(Arg,Postfix,Tail); Tail = [Arg|Postfix] ),
    append( Prefix, Tail, Nrgs ),
    New =.. [Name|Nrgs].
