%% compound( +Term, -Name, -Args ).
%
%  Generalises =.. and compound_name_arguments/3 in that it also allows atomic Term.
%
% v0.2 allows atomic Term, which breaks backward compatibility (previously these will fail).
%
% Examples
%==
% ?- compound( abc, Name, Args ).
% Name = abc,
% Args = [].
%  
% ?- compound(abc(a,b,c), Name, Args).
% Name = abc,
% Args = [a, b, c].
% 
% ?- compound( Term, abc, [a,b,c] ).
% Term = abc(a, b, c).
% 
% ?- compound( Term, abc, [] ).
% Term = abc().
%
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/1/10 ~
% @version  0.2 2023/8/31, allows atomic Term
%
compound( Term, Name, Args ) :-
	current_predicate( compound_name_arguments/3 ),
	!,
	( (compound(Term) ; (ground(Name),is_list(Args))) ->
	     compound_name_arguments( Term, Name, Args )
          ;
          atomic(Term),
          Name = Term, Args = []
     ).
compound( Term, Name, Args ) :-
	once( (compound(Term) ; (ground(Name),ground(Args))) ),
	Term =.. [Name,Args].
