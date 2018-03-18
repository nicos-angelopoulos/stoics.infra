:- module( options, [ 
              options/2, 
              options/3,
              options_append/3, 
              options_append/4,
              options_call/2, options_call/3,
              options_debug_topic/3,
              options_debug/3, 
              options_propagate/4,
              options_restore/2,
              options_return/2,
              options_version/2
            ] ).

/**  <module> Options handling and processing.

This is a stoics.infrastructure pack for handling option arguments.
The main concept is to treat options as naive Prolog lists which the
programmer can manipulate and specialise if they need to, while providing a
small number of predicates that manage basic common operations on options.
Options are particularly important in the context of SWI packs, as making
code publicly available to others often involves allowing for variations
in the behaviour of the code.

The library provides simple extensions to the basic list manipulation
predicates. In many cases it is just the error handling that is the main
difference to standard predicates.

Technically the library is designed on the semantics of memberchk/2. 
Looking for an Option in a list of options, memberchk/2 will return the
leftmost match. Library(options) sees options as a concatenation (append/3)
of the user provided options (arguments for hereon) and the defaults
provided by the predicate.

The default option values for a predicate are given by a predicate
of the same name but postfixed by '_defaults'. The library also allows for
reading user specific default options by reading profiles from a file
located at $HOME/.pl/<pred_name>.pl, if that file exists. Each options file
should have a number of option terms given as facts.

Some distinctive features of pack(options)

  * minimal "magic" behind the scenes

  * allows sloppy (un-listed) single option argument

  * defaults might depend on input

  * can-do cascading  (options of distinct predicates should be disjoint)

  * processing debug(Dbg) terms which optionise calls to debug/1

  * uniform access to user specific file-based default options

For an example see program options_example_sort_defaults.pl in examples directory.

==
?- edit( pack(options/examples/ex_sort) ).

?- [pack(options/examples/ex_sort)].

?- ex_sort( [a,b,e,c,b], Ord, true ).
Ord = [a, b, c, e].
?- ex_sort( [a,b,e,c,b], Ord, debug(true) ).
% Input list length: 5
% Output list length: 4
Ord = [a, b, c, e].
?- ex_sort( [a,b,e,c,b], Ord, order(>) ).
Ord = [e, c, b, a].
?- ex_sort( [a,b,e,c,b], Ord, duplicates(true) ).
Ord = [a, b, b, c, e].
==

Create file $HOME/.pl/ex_sort.pl with content
order(>). 

==
?- ex_sort( [a,b,e,c,b], Ord, true ).
Ord = [e, c, b, a].
==
Default for user is now order(>) which can still be over-ridden at invocation
==
?- ex_sort( [a,b,e,c,b], Ord, order(<) ).
Ord = [a, b, c, e].
==

---++ Pack info

Predicates

  * options/2,3
  * options_append/3,4
  * options_propagate/4
  * options_restore/2
  * options_return/2

Thanks to Jan Wielemaker for fixing an issue with expanding the $HOME variable and 
missing curly brackets in the errors DCG (2016/11/14).

@author   nicos angelopoulos
@version  0.2.0 2015/7/5
@version  0.4.0 2016/2/29
@version  0.5.0 2017/3/10
@version  1.0   2017/3/10
@see http://www.stoics.org.uk/~nicos/sware/options

*/

:- use_module(library(lib)).
:- lib(source(options), homonyms(true)).
        % this shouldn't be necessary but SWIPL (at least 7.3.31-9)
        % "looses" the prolog_load_context( module, _ ) value when 
        % requires/1 is used from auxilary files within the pack

:- lib(pack_errors).

:- lib(options_auxils/0).
:- lib(options_append/4).
:- lib(options_append/4).
:- lib(options_debug/3).
:- lib(options_propagate/4).
:- lib(options_restore/2).
:- lib(options_return/2).
:- lib(options_call/2).    % and /3.
:- lib(options_errors/0).
:- lib(options_debug_topic/3).
:- lib(end(options)).

/** options_version( -Version, -Date ).

Current version and release date for the library.

Currently:
==
?- options_version( Vers, Date ).
Date = date(2018,3,18),
Vers = 1:0:0.
==
*/
% options_version( 0:4:0, date(2016,2,29) ).
% options_version( 0:5:0, date(2017,3,10) ).
options_version( 1:0:0, date(2018,3,18) ).

% options_defaults( [rem_opts(_),en_list(false),ground(false)] ).
options_defaults( [en_list(false),ground(false)] ).

%% options( +Required, +Opts ).
%% options( +Required, +Opts, +OptionsOpts ).
%
% This should more naturally be option/2 but as this might cause confusion with
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
    select( Termplate, Opts, Rest ),
    !,
    memberchk( ground(Ground), OwnOpts ),
    ( ground(Termplate) -> IsGround = true; IsGround = false ),
    option_arg_ground( IsGround, Ground, Termplate, Opts ),
    option_arg_enlist( Enlist, Termplate, Needed ).
options_option_required( Opts, Needed, _Enlist, _OwnOpts, _Rest ) :-
    throw( pack_error(options,opt_required(Needed,Opts)) ).

/** option_arg_ground( +Ground, ?Termlate ).

Check groundness of found Option.

*/
option_arg_ground( true, _Ground, _Termlate, _Opts ).
option_arg_ground( false, Ground, Termlate, Opts ) :-
    options_arg_non_ground( Ground, Termlate, Opts ).

% options_arg_non_ground( true, Termlate, Opts ) :- fail
options_arg_non_ground( false, _Termlate, _Opts ).  % Termlate is NOT required to be ground
options_arg_non_ground( error, Needed, Opts ) :-
    throw( pack_error(options,opt_mustbe_ground(Needed,Opts)) ).

option_arg_enlist( true, Termlate, Needed ) :-
    Termlate =.. [Name,Arg1|Args],
    options_en_list( Arg1, Enlist1 ),
    Needed =.. [Name,Enlist1|Args].
option_arg_enlist( false, Needed, Needed ).
