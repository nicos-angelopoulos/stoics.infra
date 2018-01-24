%% term_type( +Term, -Type ).
% 
%  Type is the type of Term. One of: _var_, _list_, _compound_, _string_, _dict_, 
%  number(_integer_), number(_float_), number(rational) and _atom_.
%  
% Top: document the order
%
%==
% ?- term_type( [a,b,c], Type ).
% Type = list.
%  
% ?- term_type( a(b), Type ).
% Type = compound.
%==
%
% @author nicos angelopoulos
% @version  0.1 2017/11/21,  copy from pack(term_type) % which is currently private
% @tbd  implement in C ? (if it is faster...)
% 
term_type( Term, Type ) :-
	( var(Term) -> 
		Type = var
	;
	( (Term = [_|_];Term==[]) ->
		Type = list
	;
    ( Term = _X rdiv _Y ->   % this is new to this predicate ... we can test X&Y for being integer
        Type = number(rational)
    ;
	( number(Term) ->
        ( integer(Term) ->
		    Type = number(integer)
            ;
		    Type = number(float)
        )
	; 
	( atom(Term) ->
		Type = atom
	;
	( string(Term) ->
		Type = string
	; 
	% ( (current_predicate(is_dict/1),is_dict(Term)) -> % not in Yap
	( is_dict(Term) ->
		Type = dict
	;
	( compound(Term) ->
		Type = compound
	;
		throw( untypable_term(Term) )
	) ) ) ) ) ) ) ).
