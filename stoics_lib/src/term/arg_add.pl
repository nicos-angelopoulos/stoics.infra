/** arg_add( +N, +Term, +Arg, -New ).

	Add an Arg to the Nth position of Term, resulting in New.
    N is an arithmetic expression.

@author nicos angelopoulos
@version  0.1 2016/10/28
@version  0.2 2018/12/4, add expressions for N, added to stoics_lib

*/
arg_add( Nin, Term, Arg, New ) :-
    N is Nin,
	Term =.. [Name|Args],
	M is N - 1,
	length( Prefix, M ),
	once( append(Prefix,Postfix,Args) ),
	append( Prefix, [Arg|Postfix], Nrgs ),
	New =.. [Name|Nrgs].
