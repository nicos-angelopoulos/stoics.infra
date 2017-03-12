%% at_con( ?List, Atom ).
%% at_con( ?List, +Sep, ?Atom ).
% 
% 	Mostly a nickname for atomic_list_concat/3  because this is 
%    a well used predicate and the system name is too long, but also
%
%   * allows 
% 	for operators in Sep and a variable Sep get instantiated to '_'. 
%    (Some operators are now allowed in main atomic_list_concat/3, 
%     but this was not the case before).
% 
%   * if List contains '' that is skipped before calling the atomic_list_concat/3.
%   
%   * Sep = ''  is allowed in -List +Atom modality and returns the list of each length 1 sub atoms of Atom.
% 
%   * Sep = '' is allowed with partially instantiated List, and it allows to chop specific bits correctly
%   
% 
% ==
% ?- at_con( [a,b,c], _, Abc ).
% Abc = a_b_c.
% ?- at_con( [a,b,'',c], -, Abc ).
% Abc = 'a-b-c'.
% ?- at_con( Parts, '', abc ).
% Parts = [a, b, c].
% 
% ?- at_con( [A,orf,C], '', 'C14orf38' ).
% A = 'C14',
% C = '38' ;
% false.
% 
% ?- at_con( [A,B,C], '', abc ), write( A:B:C ), nl, fail.
% : : abc
% :a:bc
% :ab:c
% :abc:
% a: : bc
% a:b:c
% a:bc:
% ab: : c
% ab:c:
% abc: : 
% ==
%
% @author nicos angelopoulos
% @version  0.2 2014/7/15  added avoidance of ''
% @version  0.3 2014/7/15  now allows non-ground List with Sep = ''
%

at_con( List, Atom ) :-
	at_con( List, '', Atom ).
at_con( ListPrv, SepPrv, Atom ) :-
	at_con_separator( SepPrv, Sep ),
	at_con_list( ListPrv, List ),
	seperator_at_con( Sep, List, Atom ).

seperator_at_con( '', List, Atom ) :-
	var( List ),
	!,
	findall( Sub, sub_atom(Atom,_,1,_,Sub), List ).
seperator_at_con( Sep, List, Atom ) :-
	ground( List ),
	!,
	atomic_list_concat( List, Sep, Atom ).
seperator_at_con( '', List, Atom ) :-
	is_list( List ),   % non-ground list
	!,
	at_con_non_ground_list( List, Atom ).
% to get the error: 
seperator_at_con( Sep, List, Atom ) :-
	atomic_list_concat( List, Sep, Atom ).

at_con_non_ground_list( [], '' ).
at_con_non_ground_list( [H|T], Atom ) :-
	atom_concat( H, R, Atom ),
	at_con_non_ground_list( T, R ).


at_con_list( Prv, List ) :-
	% var( Prv ),
	\+ ground( Prv ),
	!,
	List = Prv.
at_con_list( Prv, List ) :-
	at_con_list_1( Prv, List ).

at_con_list_1( [], [] ).
at_con_list_1( [H|T], List ) :-
	at_con_head_list( H, List, Tail ),
	at_con_list_1( T, Tail ).

at_con_head_list( '', List, List ) :- !.
at_con_head_list( Elem, [Elem|List], List ).

at_con_separator( Provisional, Sep ) :-
	var( Provisional ),
	!,
	Sep = '_'.
at_con_separator( Provisional, Sep ) :-
	( current_op(_,_,xfx);current_op(_,_,yfx) ),
	!,
	term_to_atom( Provisional, Sep ).
at_con_separator( Sep, Sep ) :- !.
