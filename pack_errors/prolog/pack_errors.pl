:- module( pack_errors, [
                pack_errors/0,              % 
                caught/3,                   % +Goal, +Error, +Opts 
                ground/2, ground_binary/2,  % +Term, -Groundness
                defined/2, defined/3,       % +Pid,  +From[, +Opts]
                throw/2,                    % +Error,+Opts
                type/2,                     % +Type, +Term
                type/3,                     % +Type, +Term, +Opts
                of_same_length/1,           % +Lists
                of_same_length/2,           % +List1, +List2; +Lists, +Opts
                of_same_length/3,           % +List1, +List2, +Opts
                pack_errors_version/2       % +Version, +Date
                        ] ).

:- use_module( library(error) ).            % is_of_type/2.

/**  <module> Contextual error handling for packs

This is a stoics.infrastructure pack for 
  1. mid-level management handling of pack errors, 
  2. provide a simple, uniform way for informing users where the errors come from, and 
  3. provide useful pre-canned errors.

As of 0.3 the library also provides type errors on top of must_be/2

The main aim is to create contextual version of messages that can be used 
from different packs. In addition the library has evolved to provide some error related predicates.

---+++ Wrapper errors

Providing the context:
  * pack_error(Pack,Pred,Vrb,Message)
     spew Message in the context of Pack and Predicate. Vrb controls printing of context info.
     when Vrb is false there is no 
     message about the pack the error originated from
  * pack_error(Pack,Pred,Message)
     as above 
     with Vrb = true
  * pack_error(Pack,Message)
     as above without Predicate context

---+++ Prepacked errors

Argument errors (printing of Arg itself can be surpressed with prolog_flag(pack_errors_arg,false)- useful for long data).
Poss is a list of positions and Args a list of arguments.
  * arg_enumerate(Pos,Vals,Arg)
     see also type/3

  * arg_ground(Pos,Arg), 

  * arg_ground_in_one_of(Poss,Args)

  * arg_ground_pattern(Poss,Args)

Other errors 

  * cast(Term,From,To) )
  * lengths_mismatch(Tkn1,Tkn2,Len1,Len2)
  * lengths_mismatch(Tkn1,Tkn2,Op,Len1,Len2)
  * type_error(Type,Term)
  * type_error(Pos,Type,Term)
  * unknown_token(Tkn,Cat)

---+++ Examples

==
?- throw( pack_error(lengths_mismatch(a,b,1,2),[pack(mypack)]) ).
ERROR: mypack:_Unk: Lists for a and b have mismatching lengths: 1 and 2 respectively
?- throw( pack_error(arg_ground(3,name),[pack(true)]) ).
ERROR: true:_Unk: Ground argument expected at position: 3,  (found: name)

?- set_prolog_flag(pack_errors_arg,true).
?- throw( pack_error(os,arg_ground(3,name(_))) ).
ERROR: pack(os): Ground argument expected at position: 3 (found: name(_2064))

?- set_prolog_flag(pack_errors_arg,false).
?- throw( pack_error(arg_ground(3,name),[pack(true)]) ).
ERROR: true:_Unk: Ground argument expected at position: 3

?- set_prolog_flag(pack_errors_arg,true).
?- throw( pack_error(arg_enumerate(3,[a,b,c],d), [pack(os),pred(os_pred/3)]) ).
ERROR: os:os_pred/3: Term at position: 3, is not one of: [a,b,c], (found: d)

?- throw( pack_error(arg_enumerate(3,[a,b,c],d), os:os_pred/3) ).
ERROR: os:os_pred/3: Term at position: 3, is not one of: [a,b,c], (found: d)

?- throw( pack_error(arg_enumerate(3,[a,b,c],d), os_pred/3) ).
ERROR: _Unk:os_pred/3: Term at position: 3, is not one of: [a,b,c], (found: d)

?- throw( pack_error(cast(abc('file.csv'),atom),os:os_term/2) ).
ERROR: os:os_term/2: Cannot cast: abc(file.csv), to type: atom

==

---+++ Defining new pack errors

example file: 

==
:- multifile( pack_errors:message/3 ).

pack_errors:message( fold_data_insufficient(Dlen,N) ) -->
    ['Insufficient length of data (~d) as ~d folds are required'-[Dlen,N]].
pack_errors:message( fold_data_residual(Dlen) ) -->
    ['Residual data of length: ~d while splitting folds'-[Dlen]].
==

Once the above has been loaded, try with

==
?- throw( fold_data_insufficient(10,20) ).
ERROR: Insufficient length of data (10) as 20 folds are required
?- throw( pack_error(mlu,fold_data_insufficient(10,20) ) ).
ERROR: pack(mlu): Insufficient length of data (10) as 20 folds are required
?- throw( pack_error(mlu,k_fold_learn/4,fold_data_insufficient(10,20) ) ).
ERROR: mlu:k_fold_learn/4: Insufficient length of data (10) as 20 folds are required
==

---+++ Pack info

The library listens to =|debug(pack_errors)|=.

@author  nicos angelopoulos
@version 0.1 2016/01/30
@version 0.2 2016/02/24
@version 0.3 2017/03/06
@version 1.0 2018/03/18
@see     http://stoics.org.uk/~nicos/sware/pack_errors
@tbd     equal length list checking
@see     lib predicates:
@see     caught/3                   args, +Goal, +Call, +Opts
@see     ground/2, ground_binary/2  args: +Term, -Groundness
@see     throw/2                args: +Ball, +Opts
@see     type/2,  type/3        args: +Type, +Term [, +Opts]
@see     pack_errors/0, pack_errors_version/2  args: +Version, +Date
@see     defined/3              args: 

*/

:- multifile prolog:message//1.

caught_defaults( [report(error)] ).

/** caught( +Goal, +Error, +Opts ).

Catches all errors and failure of Goal. The idea is that
all non-successful executions are handled identical by the call.
If Goal errors, the primary thrown ball is caught and discarded. 
If Goal errors or fails, behaviour depends on option value Report (see Opts below).

Opts 

  * report(Report=error)
   * ignore 
        ignores by reporting nothing and succeeding
   * fail
        reports nothing but call itself fails
   * error
        throws the error (any unrecognised value defaults to error)

  * ball(Ball)
      instantiates the original exception Ball caught from calling Goal.
      (So that parts of it can be included in Error.)

==
?- caught( fail, my_exception(on_data), true ).
ERROR: Unhandled exception: my_exception(on_data)

?- caught( fail, my_exception(on_data), [report(ignore)] ).
true

?- caught( fail, my_exception(on_data), [report(fail)] ).
false

?- caught( xyz, my_except(Ball), [ball(Ball)] ).
ERROR: Unhandled exception: my_except(error(existence_error(procedure,pack_errors:xyz/0),context(system:catch/3,_11198)))
==

@see throw/2 

*/
caught( Goal, Error, Args ) :-
    pack_errors_options_append( caught, Args, Opts ),
    caught_opts( Goal, Error, Opts ).

caught_opts( Goal, Error, Opts ) :-
    catch( Goal, Ball, pack_errors:caught_error(Goal,Ball,Error,Opts) ),
    !.
caught_opts( _Goal, Error, Args ) :-
    pack_errors_options_append( caught, Args, Opts ),
    caught_opt_throw( OnThrow, Opts ),
    throw( Error, on_throw(OnThrow) ).

caught_error( _Goal, Ball, Error, Opts ) :-
    memberchk( ball(Ball), Opts ),
    !,
    caught_opt_throw( OnThrow, Opts ),
    throw( Error, on_throw(OnThrow) ).
caught_error( _Goal, _Ball, Error, Opts ) :-
    caught_opt_throw( OnThrow, Opts ),
    throw( Error, on_throw(OnThrow) ).

caught_opt_throw( OnThrow, Opts ) :-
    memberchk( report(Rep), Opts ),
    caught_opt_report_throw( Rep, OnThrow ).

caught_opt_report_throw( ignore, succeed ) :-
    !.
caught_opt_report_throw( Rep, Rep ).

/** ground( +Term, -Groundness ).
    ground_binary( +Term, -Groundness ).

Instantiates groundness of Term to Type. In ground_binary/2
Groundness =partial= and =false= are collapsed to =false=.

Groundness
  * true
    Term is ground

  * false
    Term is variable

  * partial
    Term is partially instantiated

==
?- ground( abc, Abc ), ground( de(F), Def ), ground( GHI, Ghi ).
Abc = true,
Def = partial,
Ghi = false.

?- ground_binary( abc, Abc ), ground_binary( de(F), Def ), ground_binary( GHI, Ghi ).
Abc = true,
Def = Ghi, Ghi = false.
*/
ground( Term, Type ) :-
    var( Term ),
    !,
    Type = false.
ground( Term, Type ) :-
    ground( Term ),
    !,
    Type = true.
ground( _Term, Type ) :-
    Type = partial.
    
ground_binary( Term, Type ) :-
    ground( Term ), 
    !,
    Type = true.
ground_binary( _Term, false ).

throw_defaults( [on_throw(error),pack_format(short),as_pack_err(true),severity(error)] ).

/** throw( +Error, +Opts ).

An optionised version of throw/1. Error is not thrown if OnThrow==fail 
(see Opts below) and the call to throw/2 itself fails. When =|OnThrow==succeed|=
Error is not thrown and the call itself succeeds.
For all other values the default behaviour is that of =|OnThrow==error|=
where is to thrown Error is assumed.

As of version 0.3 this should be the adviced entry point for throwing pack tracing balls.

Opts
 * on_throw(OnThrow=error)
    one of [succeed,fail,error]. (error is currently a bit of a misnomer, it should be throw)

 * as_pack_err(Perr=true)
    true wraps Error, as a pack_error

 * pack(Pack=_)
    originator pack
 
 * pred(Pred=_)
    originator predicate

 * pack_format(Pfmt=short)
    output format for pack announcement

 * severity(Severity=error)
    Severity is passed to print_message/2 (first argument)

==
?- throw( my_error(x), true ).
ERROR: Unhandled exception: my_error(x)

?- throw( my_error(x), on_throw(succeed) ).
true.

?- throw( my_error(x), on_throw(fail) ).
false.

?- throw( my_error(x), on_throw(error) ).
ERROR: Unhandled exception: my_error(x)

?- throw( my_error(x), on_throw(whatelse) ).
ERROR: Unhandled exception: my_error(x)

==

@author nicos angelopoulos
@version  0.2 2017/3/6
@version  0.3 2018/1/5  added tracer options: pack, pred & pack_format

*/
throw( Error, Args ) :-
    pack_errors_options_append( throw, Args, Opts ),
    memberchk( on_throw(OnThrow), Opts ),
    throw_if_on( OnThrow, Error, Opts ).

throw_if_on( succeed, _Error, _Opts ) :- 
    !.
throw_if_on( fail, _Error, _Opts ) :-
    !,
    fail.
throw_if_on( _, Error, Opts ) :-
    memberchk( as_pack_err(Per), Opts ),
    throw_as_pack_error( Per, Error, Opts ).

throw_as_pack_error( false, Error, _Opts ) :-
    !,
    throw( Error ).
throw_as_pack_error( _, Error, Opts ) :-
    memberchk( pack_format(Sil), Opts ),
    memberchk( severity(Lvl), Opts ),
    ( memberchk(pack(Pack),Opts) -> 
        ( memberchk(pred(Pred),Opts) ->
            true
            ;
            Pred = '$unknown'/0
        ),
        % throw( pack_error(Pack,Pred,Sil,Error) )
        throw_level( Lvl, pack_error(Pack,Pred,Sil,Error), Opts )
        ;
        ( memberchk(pred(Pred),Opts) ->
            Pred = Pname/Arity,
            throw_level( Lvl, pack_error(Pname/Arity,Error), Opts )
            ;
            throw_level( Lvl, pack_error(Error), Opts )
        )
    ).

throw_level( Lvl, BallMark, Opts ) :-
    ( BallMark =.. [pack_error,Barg] ->
        Ball =.. [pack_error,Barg,Opts]
        ;
        ( BallMark =.. [pack_error,_Barg,_Opts] ->
            Ball = BallMark
            ;
            Ball = pack_error(BallMark,Opts)
        )
    ),
    debug( pack_errors, 'Leveled ball: ~w', Ball ),
    prolog:message( Ball, Mess, [] ), % i thinks [] is correct
	print_message_lines( current_output, kind(Lvl), Mess ).

type_defaults( [error(true),pack(false),pred(false),arg(false)] ).

/** type( +Type, @Term ).
    type( +Type, @Term, +Opts ).

type/2 is a superset of must_be, in that it adds Type = @(Callable), (equiv: Type = call(Callable)), which will succeed iff
call( Callable, Term ) succeeds. It also enhances must_be/2 by adding options.
In the case of a call-wrapped type, the call to type/3 will succeed iff 
call(Callable,Term) succeeds.

Opts (unlisted is ok)

  * error(Err=true)
      when false, call fails 
      instead of throwing error

  * arg(Err=true)
      some argument position of Term.
      (false is reserved and prints no info.)

  * pack(Pack=false)
      when given, the error contains info on the pack throwing the error
      (false is reserved and prints no info)

  * pred(Pred=false)
      when given, the error contains info on the predicate throwing the error
      (false is reserved and prints no info)

==
?- type( boolean, maybe ).
ERROR: Object of type: boolean, expected but found term: maybe
?- type( boolean, maybe, error(false) ).
false.
?- type( boolean, maybe, pack(sure) ).
ERROR: pack(sure): Object of type: boolean, expected but found term: maybe
?- type( boolean, maybe, [pack(sure),pred(lost/2)] ).
ERROR: sure:lost/2: Object of type: boolean, expected but found term: maybe
?- type( boolean, maybe, [pack(sure),pred(lost/2@3)] ).
ERROR: Syntax error: Operator expected
ERROR: type( boolean, maybe, [pack(sure),pred(lost/
ERROR: ** here **
ERROR: 2@3)] ) . 
?- type( boolean, maybe, [pack(sure),pred(lost/2+3)] ).
ERROR: sure:lost/2+3: Object of type: boolean, expected but found term: maybe
?- type( boolean, maybe, [pack(sure),pred(1+lost/2)] ).
ERROR: sure:1+lost/2: Object of type: boolean, expected but found term: maybe
?- type( boolean, maybe, [pack(sure),pred(lost(arg1)/2)] ).
ERROR: sure:lost(arg1)/2: Object of type: boolean, expected but found term: maybe
==
*/
type( Type, Term ) :-
    type( Type, Term, [] ).

type( Type, _Term, _Args ) :-
    \+ ground( Type ),
    !,
    throw( pack_error(pack_error,type/3,arg_ground(1,Type)) ).
type( Type, Term, Args ) :-
    pack_errors_options_append( type, Args, Opts ),
    type_optioned( Type, Term, Opts ).

type_optioned( @(GoalPrv), Term, _Opts ) :-
    ( GoalPrv = _:_ -> Goal = GoalPrv; Goal = user:GoalPrv ),
    call( Goal, Term ),
    !.
type_optioned( call(GoalPrv), Term, _Opts ) :-
    ( GoalPrv = _:_ -> Goal = GoalPrv; Goal = user:GoalPrv ),
    call( Goal, Term ),
    !.
type_optioned( Type, Term, _Opts ) :-
    Type \= call(_),
    is_of_type( Type, Term ),
    !.
type_optioned( Type, Term, Opts ) :-
    memberchk( error(Error), Opts ),
    type_error( Error, Type, Term, Opts ).

type_error( false, _Type, _Term, _Opts ) :- !, fail.
type_error( true, Type, Term, Opts ) :-
    memberchk( pack(Pack), Opts ),
    memberchk( pred(Pred), Opts ),
    memberchk( arg(Pos), Opts ),
    type_error_position( Pos, Pack, Pred, Type, Term ).

type_error_position( false, Pack, Pred, Type, Term ) :-
    !,
    throw( pack_error(Pack,Pred,type_error(Type,Term)) ). 
type_error_position( Pos, Pack, Pred, Type, Term ) :-
    throw( pack_error(Pack,Pred,type_error(Pos,Type,Term)) ). 

pack_errors_options_append( Pname, ArgS, Opts ) :-
    ( is_list(ArgS) -> Args = ArgS ; Args = [ArgS] ),
    atom_concat( Pname, '_defaults', Dname ),
    Dcall =.. [Dname,Defs],
    call( Dcall ),
    append( Args, Defs, Opts ),
    !.

defined_defaults( [load(false)] ).

/** defined( +Pid, +From, +Opts ).

Throws an error if Pid is not defined in current context.<br>
From is the source from where Pid was supposed to be loaded.<br>
This predicate can act independently (particularly with load(true))<br>
or be combined with pack(lib)'s lib(suggests(Pack)) to, on-demand, <br>
pinpoint to which library is missing and what <br>
predicate within that pack is the deal breaker.

Note that pack(lib) also provides
==
lib(suggests(Pid,Load))
==
which is an alternative and more automatic way to achieve demand driven 
loading via hot-swapping.

==
:- lib(suggests(Pack))
==
silently fails if Pack is not present. This is intendent for dependendencies 
that do not impact major parts for the importing pack. Thus allow common
use without grabbing all dependencies that may not be needed for a particular user.

Opts are passed to throw/2, except for:
 * load(Load=false)

==
?- defined( abc/0, pack(b_real) ).
ERROR: Predicate: abc/0 is not defined (source apparently available at: pack(b_real); not asked to load)

?- defined( abc/0, false ).
ERROR: Predicate: abc/0 is not defined

?- defined( abc/0, false, pack(sourcey) ).
ERROR: sourcey:$unknown/0: Predicate: abc/0 is not defined

?- defined( abc/0, pack(b_real), [pack(sourcey),pred(foo/1;2)] ).
ERROR: sourcey:foo/1;2: Predicate: abc/0 is not defined (source apparently available at: pack(b_real); not asked to load)

?- defined( b_real/0, pack(b_real), [as_pack_err(true),load(library(b_real))] ).
true.
==
The above only succeeds if b_real is an install library and defines b_real/0.

From or Load can have the special form: lib(CodeLib). This assumes pack(lib) is installed and lib/1
will be used to load the requested CodeLib.
==
?- defined( b_real/0, lib(b_real), load(true) ),
==
Will again, only succeed if b_real is installed and defines b_real/0. In this occasion library(lib) should be also installed.

@author nicos angelopoulos
@version  0.1 2018/1/5
@see throw/2
@see lib/1 (lib(suggests/1)) can work with this predicate
@see lib/1 (lib(suggests/2)) as an alternative

*/
defined( Pid, From ) :-
    defined( Pid, From, [] ).
defined( Pid, _From, _Opts ) :-
    current_predicate( Pid ),
    !. % fixme: need version where From and Into are checked ? 
       %        here we don't check as From and Into are assumed as tracers no enforcables
defined( Pid, From, ArgS ) :-
    \+ var(ArgS),                   % fixme: error
    defined_defaults( Defs ),
    ( is_list(ArgS) -> Args = ArgS; Args = [ArgS] ),
    append( Args, Defs, Opts ),
    memberchk( load(Load), Opts ),
    defined_if_load( Load, Pid, From, Args ).

defined_if_load( false, Pid, From, Opts ) :-
    throw( expected_from(false,Pid,From), Opts ).
defined_if_load( true, Pid, From, Opts ) :-
    !,
    defined_load( From, Pid, From, Opts ).
defined_if_load( Other, Pid, From, Opts ) :-
    defined_load( Other, Pid, From, Opts ).

defined_load( lib(This), Pid, From, Args ) :-
    !,
    lib:lib(This),
    defined_loaded( Pid, From, Args ).
defined_load( LoadThis, Pid, From, Args ) :-
    % fixme: check is not loaded ?
    user:ensure_loaded( LoadThis ),
    defined_loaded( Pid, From, Args ).

defined_loaded( Pid, _From, _Opts ) :-
    current_predicate( user:Pid ),
    !. % fixme: need version where From and Into are checked ? 
defined_loaded( Pid, From, Opts ) :-
    throw( expected_from(true,Pid,From), Opts ).

/** pack_errors.

This is a documentation predicate, providing an anchor for documentation pointers.

*/
pack_errors :-
    write( 'Contextual error handling for packs' ), nl.

/** of_same_length( +Lists ).
    of_same_length( +List1, +List2 ).
    of_same_length( +Lists, +Opts ).
    of_same_length( +List1, +List2, +Opts ).
    
Generic sanity predicate, checking that two or more lists are of the same length.

In order to disambiguate between the two versions of the arity 2,
in that scenario options should be a term of the form opts(OptsL).

Opts are passed to throw/2, the only local one is:
  * action(Act)
     * error
        prints error and fails (default)
     * throw
        throws a tuntrum of the form, of_same_length(Lng1,Lng2,Tkn1,Tkn2)
     * warning(Tkn)
        prints warning but succeeds
     * warn_lists(Tkn)      
        prints warning that includes lengths (?)
  * token1(Tkn1=1)
     name of List1 (used in error throwing to id the list)
  * token1(Tkn1=n)
     name of List2 (used in errors; 
     in Lists scenario it will be the first list length-mismatch the first list)

==
?-
    of_same_length( [a,b,c], [1,2,3] ).

true.

?- of_same_length( [[a,b,c],[1,2,3]] ).

?- 
    of_same_length( [1,2,3], [a,b], token1(first) ).

ERROR: Lists for first and 2 have mismatching lengths: 3 and 2 respectively

==
@author nicos angelopoulos
@version  0.1 2018/09/24

*/

of_same_length( [List1|Lists] ) :-
    of_same_length_1( Lists, List1, [] ).

of_same_length( [List1|Lists], opts(Opts) ) :-
    !,
    of_same_length_1( Lists, List1, Opts ).
of_same_length( List1, List2 ) :-
    of_same_length_1( [List2], List1, [] ).
of_same_length( List1, List2, Opts ) :-
    of_same_length_1( [List2], List1, Opts ).

of_same_length_1( Lists, List1, Args ) :-
    length( List1, Lng1 ),
    \+ var(Args),
    ( is_list(Args) -> append(Args,[action(throw)],Opts) ; Opts = [Args,action(throw)] ),
    memberchk( action(Act), Opts ),
    of_same_length_1( Lists, 2, Lng1, List1, Act, Opts ).

% currently List1 is not used, but it could be passed to 
% of_same_length_mismatch for reporting of clashing lists...
of_same_length_1( [], _I, _Lng, _List1, _Act, _Opts ).
of_same_length_1( [HList|T], I, Lng1, List1, Act, Opts ) :-
     length( HList, HLng ),
     ( HLng =:= Lng1 ->
          true
          ;
          % throw( not_of_equal_length(HLng,Lng) )
          ( memberchk(token1(Tkn1),Opts) -> true; Tkn1 = 1 ),
          ( memberchk(token2(Tkn2),Opts) -> true; Tkn2 = I ),
          of_same_length_mismatch( Act, Lng1, HLng, Tkn1, Tkn2, Opts )
     ),
     J is I + 1,
     of_same_length_1( T, J, Lng1, List1, Act, Opts ).

of_same_length_mismatch( error, Lng1, Lng2, Tkn1, Tkn2, Opts ) :-
    throw( lengths_mismatch(Tkn1,Tkn2,Lng1,Lng2), Opts ).
of_same_length_mismatch( fail, _Lng1, _Lng2, _Tkn1, _Tkn2, _Opts ) :-
    fail.
% throw_lists ? which will also include the offender & base lists ?
of_same_length_mismatch( throw, Lng1, Lng2, Tkn1, Tkn2, _Opts ) :-
    throw( of_same_length(Lng1,Lng2,Tkn1,Tkn2) ).
of_same_length_mismatch( warning, Lng1, Lng2, Tkn1, Tkn2, _Opts ) :-
    % % Format = 'Lists at:~w and ~w, have differing lengths: ~d and ~d',
    % % message_report( Format, [Tkn1,Tkn2,Lng1,Lng2], informational ).

    throw( lengths_mismatch(Tkn1,Tkn2,Lng1,Lng2), severity(warning) ).

    % message( lengths_mismatch(Tkn1,Tkn2,Lng1,Lng2), List, [] ),
	% print_message_lines(current_output, kind(warning), List ).
% of_same_length_mismatch( warning, Lng1, Lng2, Tkn1, Tkn2, _Opts ) :-

of_same_length_mismatch( error, Lng1, Lng2, Tkn1, Tkn2, _Opts ) :-
    Format = 'Lists at:~p, have differing lengths, ~d and ~d',
    message_report( Format, [Tkn1,Tkn2,Lng1,Lng2], error ),
    fail.
of_same_length_mismatch( warning(Tkn), Lng1, Lng2, _L1, _L2, _Opts ) :-
    Format = 'Lists at:~p, have differing lengths, ~d and ~d',
    message_report( Format, [Tkn,Lng1,Lng2], informational ).
of_same_length_mismatch( warn_lists(Tkn), Lng1, Lng2, L1, L2, _Opts ) :-
    Format = 'Lists at:~p, have differing lengths, ~d and ~d. The lists are as follows',
    Args   = [Tkn,Lng1,Lng2],
    message_report( Format, Args, informational ),
    Format1 = 'Length mismatch list1:~p',
    message_report( Format1, [L1], debug(_) ),
    Format2 = 'Length mismatch List2:~p',
    message_report( Format2, [L2], debug(_) ).

pack_message_options_augment( Opts, Apts ) :-
    ( select(Mod:Pred,Opts,Rem) ->
        Apts = [pred(Mod:Pred)|Rem]
        ;
        ( select(Name/Arity,Opts,Rem) ->
            Apts = [pred(Name/Arity)|Rem]
            ;
            Apts = Opts
        )
    ).

/** pack_errors_version( -Version, -Date ).

Current version and release date for the library.

==
?- pack_errors_version( 0:3:0, date(2017,3,6) ).
==
*/

% pack_errors_version( 0:3:0, date(2017,3,6) ).
% pack_errors_version( 1:0:0, date(2018,3,18) ).
pack_errors_version( 1:0:1, date(2018,9,24) ).

% here new: 18.9.24
prolog:message(unhadled_exception(pack_error(Message))) -->
     { debug( pack_errors, 'Unhandled pack_error/1 ~w', [Message] ) },
     % pack_errors:message(Message,[]).
     pack_message(Message,[]).
prolog:message(unhandled_exception(pack_error(Message,Opts))) -->
     { debug( pack_errors, 'Unhandled pack_error/2 c ~w, ~w', [Message,Opts] ) },
     pack_message(Message,Opts).

prolog:message(pack_error(Message)) -->
     { debug( pack_errors, 'Pack_error/1: ~w', [Message] ) },
     pack_message(Message, []).
prolog:message(pack_error(Message,Opts)) -->
     { debug( pack_errors, 'Pack_error/2: ~w, ~w', [Message,Opts] ) },
     pack_message(Message, Opts).

% eoh, end of here


/*
prolog:message(unhandled_exception(pack_error(Message))) -->
     { debug( pack_errors, 'Unhandled pack_error/1 ~w', Message ) },
     pack_errors:message(Message).
prolog:message(unhandled_exception(pack_error(Pack,Message))) -->
     { debug( pack_errors, 'Unhandled pack_error/2 ~w', Message ) },
    pack_errors:message(from_pack(true,Pack,false)),
     pack_errors:message(Message).
prolog:message(unhandled_exception(pack_error(Pack,Pred,Message))) -->
     { debug( pack_errors, 'Unhandled pack_error/3 ~w', Message ) },
    message(from_pack(true,Pack,Pred)),
     message(Message).
prolog:message(unhandled_exception(pack_error(Pack,Pred,Sil,Message))) -->
     { debug( pack_errors, 'Unhandled pack_error/4 ~w', Message ) },
    message(from_pack(Sil,Pack,Pred)),
     message(Message).
prolog:message(unhandled_exception(Message) ) -->
     { debug( pack_errors, 'Unhandled error, passing as is: ~w', Message ) },
     message(Message).
prolog:message(pack_error(Message)) -->
     { debug( pack_errors, 'Pack_error/1: ~w', Message ) },
     message(Message).
prolog:message(pack_error(Pname/Arity,Message)) -->
    { debug( pack_errors, 'Pack_error/2 (first is predicate): ~w', Message ) },
    message(from_pack_pred(Pname,Arity)),
    message(Message).
prolog:message(pack_error(Pack,Message)) -->
    { debug( pack_errors, 'Pack_error/2 (x): ~w', Message ) },
    { debug( pack_errors, 'calling from_pack/3 (x)', [] ) },
    message(from_pack(true,Pack,false)),
    message(Message).
prolog:message(pack_error(Pack,Pred,Message)) -->
    { debug( pack_errors, 'Pack_error/3: ~w', Message ) },
    message(from_pack(true,Pack,Pred)),
    message(Message).
prolog:message(pack_error(Pack,Pred,Sil,Message)) -->
    { debug( pack_errors, 'Pack_error/4: ~w', Message ) },
    message(from_pack(Sil,Pack,Pred)),
    message(Message).
*/

:- multifile( pack_errors:message/3 ).

% here
pack_message( Mess, OptsPrv ) -->
    % fixme: check for var(OptsPrv) ?
    {( is_list(OptsPrv) -> OptsPrv = Opts; Opts = [OptsPrv] )},
    {pack_message_options_augment(Opts,Apts)},
    message_pack( Apts ),
    pack_errors:message( Mess ).

message_pack( Opts )  -->
    { debug( pack_errors, 'message_pack options: ~w', [Opts] ) },
    { (memberchk(pred(PredPrv),Opts)->true; PredPrv='_Unk'),
      (memberchk(pack(Pack),Opts)->
            ( PredPrv = PredMod:PredFct ->
                ( PredMod = Pack -> 
                    Pred = PredFct
                    ;
                    Pred = PredPrv   % both Pack and Mod will be displayed
                ) 
                ;
                Pred = PredPrv
            )
            ; 
            ( PredPrv = Pack:Pred ->
                true
                ;
                PredPrv = Pred,
                Pack='_Unk'
            )
      ),
      debug( pack_errors, 'pack:predicate identified as: ~w:~w', [Pack,Pred] ),
      \+ (Pack=='_Unk', Pred=='_Unk'),
      !
    },
    ['~w:~w: '-[Pack,Pred] ].
message_pack( _ )  --> [].


% eoh: end of here

% add cuts ? 
%
message( from_pack_pred(Pname,Arity) ) --> 
     { debug( pack_errors, 'From pack_pred: ~w', Pname/Arity) },
    ['~w/~w: '-[Pname,Arity] ].
message( from_pack(false,_Pack,_Pred) ) --> [].
message( from_pack(long,Pack,Pred) ) -->
    pack_errors:message( from_pack_long(Pred,Pack) ).
message( from_pack(true,Pack,Pred) ) -->
    pack_errors:message( from_pack_short(Pred,Pack) ).
message( from_pack(short,Pack,Pred) ) -->                % synonym to true
    pack_errors:message( from_pack_short(Pred,Pack) ).

message( from_pack_short(false,false) ) -->
    { ! }.
message( from_pack_short(Pred,false) ) -->
    ['~w: '-[Pred] ].
message( from_pack_short(false,Pack) ) -->
     { debug( pack_errors, 'From pack short: ~w', Pack ) },
    ['pack(~w): '-[Pack] ],
     { debug( pack_errors, 'From pack short: ~w succeeded', Pack ) }.
message( from_pack_short(Pred,Pack) ) -->
    ['~w:~w: '-[Pack,Pred] ].
    % ['[~w::~w]: '-[Pack,Pred] ].

message( from_pack_long(false,Pack) ) -->
    ['Following error generated from pack: ~w\n'-[Pack] ].
message( from_pack_long(Pred,Pack) ) -->
    ['Following error generated for predicate: ~w, from pack: ~w\n'-[Pred,Pack] ].

message( arg_enumerate(Pos,Vals,_Arg) ) --> 
    { current_prolog_flag(pack_errors_arg,false) },
    ['Term at position: ~d, is not one of: ~w'-[Pos,Vals]].
message( arg_enumerate(Pos,Vals,Arg) ) --> 
    ['Term at position: ~d, is not one of: ~w, (found: ~w)'-[Pos,Vals,Arg]].
message( arg_ground(Pos,_Arg) ) -->
    { current_prolog_flag(pack_errors_arg,false) },
    ['Ground argument expected at position: ~d'-[Pos]].
message( arg_ground(Pos,Arg) ) -->
    ['Ground argument expected at position: ~d,  (found: ~w)'-[Pos,Arg]].
message( args_ground(Pos,_Arg) ) -->
    { current_prolog_flag(pack_errors_arg,false) },
    ['Ground arguments expected at position: ~d'-[Pos]].
message( args_ground(Pos,Arg) ) -->
    ['Ground argument expected at position: ~d,  (found: ~w)'-[Pos,Arg]].
% message( arg_ground_at_either(Pos1,Pos2,_Arg1,_Arg2) ) -->  % Pos1 & Pos2 can be lists of positions
message( arg_ground_in_one_of(Poss,_Args) ) -->                     
    { current_prolog_flag(pack_errors_arg,false) },
    ['Ground argument expected in one of the positions: ~w'-[Poss]].
message( arg_ground_in_one_of(Poss,Args) ) --> 
    ['Ground argument expected in one of the positions : ~w, but found: ~w'-[Poss,Args]].
message( arg_ground_pattern(Poss,_Args) ) -->
    { current_prolog_flag(pack_errors_arg,false) },
    ['Ground arguments expected in some of the positions: ~w'-[Poss]].
message( arg_ground_pattern(Poss,Args) ) -->
    ['Ground arguments expected in some of the positions: ~w, but found:~w'-[Poss,Args]].

message( lengths_mismatch(Tkn1,Tkn2,Len1,Len2) ) -->
    ['Lists for ~w and ~w have mismatching lengths: ~d and ~d respectively'-[Tkn1,Tkn2,Len1,Len2]].
message( lengths_mismatch(Tkn1,Tkn2,Op,Len1,Len2) ) -->
    ['Terms idied by: ~w and ~w, have mismatching lengths: ~d and ~d respectively (~w expected)'-[Tkn1,Tkn2,Len1,Len2,Op]].
message( cast(Term,From,To) ) -->
    ['Cannot cast: ~w, from type: ~w to type: ~w'-[Term,From,To]].
message( cast(Term,To) ) -->
    ['Cannot cast: ~w, to type: ~w'-[Term,To]].
message( type_error(false,Type,Term) ) -->
    ['Object of type: ~w, expected but found term: ~w'-[Type,Term]].
message( type_error(Pos,Type,Term) ) -->
    ['Object of type: ~w, expected at position:~w but found: ~w'-[Type,Pos,Term]].
message( type_error(Type,Term) ) -->
    ['Object of type: ~w, expected but found term: ~w'-[Type,Term]].
message( unknown_token(Tkn,Cat) ) -->
    ['Token: ~w, is not a recognisable: ~w'-[Tkn,Cat]].
message( expected_from(_,Pid,false) ) -->
    ['Predicate: ~w is not defined'-[Pid]].
message( expected_from(false,Pid,From) ) -->
    ['Predicate: ~w is not defined (source apparently available at: ~w; not asked to load)'-[Pid,From]].
message( expected_from(true,Pid,From) ) -->
    ['Predicate: ~w is not defined (source apparently available at: ~w; which was loaded).'-[Pid,From]].
