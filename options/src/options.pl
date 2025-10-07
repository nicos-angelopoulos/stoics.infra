% options_defaults( [rem_opts(_),en_list(false),ground(false)] ).
options_defaults( [en_list(false),ground(false)] ).

%% options( +Required, +Opts ).
%% options( +Required, +Opts, +OptionsOpts ).
%
% This should more naturally be option/2 but this might cause confusion with
% Swi's own option library.
% Required can be a single or list of terms. 
%
% Opts can also be non list as it is passed through en_list/2.
% 
% Options
% * en_list(Enlist=false)
%    when true it enlists the returning argument
% * ground(Ground=false)
%    should the selected option required to be ground, and what to do if not
%    * false
%      dont do anything
%    * true
%      fail
%    * error
%      throw error
% * true(TruthValue)
%   only variable is allowed, if present the call always succeeds and TruthValue is bound to either _true_ or _false_ depending on underlying success of options/2 call
% * rem_opts(RemOpts)
%    RemOpts is Opts after all functor/shape matching mentions of Required have been removed.
%
% The predicate fails silently if the first Required option with equivalent "shape"
% in Opts fails to pattern match (unlike classic memberchk/2).
%
%==
% ?- options( x(y), [x(x)] ).
% false.
% 
% ?- options( x(y), [x(x),x(y)] ).
% false.
%
% ?- options( x(X), [a(b),c(d),x(y),x(x)] ).
% X=y.
%
% ?- options( [a(X),c(b)], [a(b),x(x),b(c)] ).
% ERROR: Required option: c(b), not present in options: [a(b),x(x)]
% % Execution Aborted
% 
% ?- options( x(X), [a(b),c(d),x(y),x(x)], rem_opts(Rem) ).
% X = y,
% Rem = [a(b), c(d)].
%
% ?- options( x(X), [a(b),c(d),x(y),x(x)], en_list(true) ).
% X= [y].
%
% ?- options( a(A), [a(X),b(c)], ground(error) ).
% ERROR: pack(options): Option should be ground, but found: a(_G1470), in options: [a(_G1470),b(c)]
% ?- options( a(A), [a(X),b(c)], ground(true) ).
% false.
% 
% ?- options( a(A), [a(X),b(c)] ).
% A = X.
% 
% ?- options( a(A), [a(X),b(c)], ground(false) ).
% A = X.
% 
% ?- options( a(b), [a(a),a(b),b(c)], true(T) ).
% T = fail.
% 
% ?- options( a(A), [a(a),a(b),b(c)], true(T) ).
% A = a,
% T = true.
% 
% ?- options( a(a), [a(a),a(b),b(c)], true(T) ).
% T = true.
% 
%==
%
% @author nicos angelopoulos
% @version  0.2 2015/01/16
% @version  0.3 2015/12/06  changed 3rd argument to Options of its own invocation
%
options( Needed, Opts ) :-
    options( Needed, Opts, [] ).

options( ReqS, OptS, OwnOptS ) :-
    options_append( options, OwnOptS, OwnOpts ),
    options_en_list( ReqS, Reqs ),
    options_en_list( OptS, Opts ),
    memberchk( en_list(Enlist), OwnOpts ),
    debug( fixme, 'Use type checking on: ~a', Enlist ),
    options_required_truth_value( Reqs, Opts, Enlist, OwnOpts, Rem ),
    (memberchk(rem_opts(Rem),OwnOpts) -> true; true ).

options_required_truth_value( Reqs, Opts, Enlist, OwnOpts, Rem ) :-
    options_required( Reqs, Opts, Enlist, OwnOpts, Rem ),
    !,
    ( memberchk(true(true),OwnOpts) -> true; true ).
options_required_truth_value( _Reqs, Opts, _Enlist, OwnOpts, Opts ) :-  % Rem = Opts here
    memberchk( true(fail), OwnOpts ).

options_required( [], Opts, _Enlist, _OwnOpts, Opts ).
options_required( [Needed|T], Opts, Enlist, OwnOpts, Rem ) :-
    options_option_required( Opts, Needed, Enlist, OwnOpts, Rest ),
    options_required( T, Rest, Enlist, OwnOpts, Rem ).

options_option_required( Opts, Needed, Enlist, OwnOpts, Rest ) :-
    functor( Needed, Tname, Tarity ),
    functor( Termplate, Tname, Tarity ),
    option_select_functor( Opts, Tname, Tarity, Sel, Rest ),
    memberchk( Termplate, Sel ),
    !,
    memberchk( ground(Ground), OwnOpts ),
    ( ground(Termplate) -> IsGround = true; IsGround = false ),
    option_arg_ground( IsGround, Ground, Termplate, Opts ),
    option_arg_enlist( Enlist, Termplate, Needed ).
options_option_required( Opts, Needed, _Enlist, _OwnOpts, _Rest ) :-
    PeOpts = [pred(options/2),pack(options)],
    options_throw( pack_error(opt_required(Needed,Opts)), PeOpts ).

/** option_arg_ground( +Ground, ?Termlate ).

Check groundness of found Option.

*/
option_arg_ground( true, _Ground, _Termlate, _Opts ).
option_arg_ground( false, Ground, Termlate, Opts ) :-
    options_arg_non_ground( Ground, Termlate, Opts ).

% options_arg_non_ground( true, Termlate, Opts ) :- fail
options_arg_non_ground( false, _Termlate, _Opts ).  % Termlate is NOT required to be ground
options_arg_non_ground( error, Needed, Opts ) :-
    PeOpts = [pred(options/2),pack(options)],
    options_throw( pack_error(opt_mustbe_ground(Needed,Opts)), PeOpts ).

option_arg_enlist( true, Termlate, Needed ) :-
    Termlate =.. [Name,Arg1|Args],
    options_en_list( Arg1, Enlist1 ),
    Needed =.. [Name,Enlist1|Args].
option_arg_enlist( false, Needed, Needed ).

option_select_functor( [], _Tname, _Tarity, [], [] ).
option_select_functor( [H|T], Tname, Tarity, Sel, Rest ) :-
    ( functor(H,Tname,Tarity) -> 
        Sel = [H|TSel],
        Rest = TRest
        ;
        Sel = TSel,
        Rest = [H|TRest]
    ),
    option_select_functor( T, Tname, Tarity, TSel, TRest ).

options_throw( Term, Opts ) :-
     ( current_predicate(throw/2) ->
          throw( Term, Opts )
          ;
          throw( Opts:Term )
     ).
