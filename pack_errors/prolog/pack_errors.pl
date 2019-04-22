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

This is a stoics.infrastructure pack that
  1. implements the mid layer for handling Prolog errors
  2. provides a simple, uniform way for displaying originating pack/module and predicate
  3. includes useful pre-canned errors
  4. incorporates error related predicates
  5. decouples the type of printing from the execution behaviour and controls both aspects
     through simple options

Version 0.3 introduced type errors via type/3 on top of must_be/2.<br>
Version 2.0 has been re-written to be Options centric, fully decoupled and introduced of_same_length/3.

The pack manage mid-level error handling in a uniform way so other packs can use SWI's infracture
in a simple way. The user only needs to define the print messages (if the pre-canned ones are not suitable)
and then throw the appropriate terms during execution.

Two simple ways to identifying originating caller are provided by allowing options in either
the message, or via using an new version of throw, throw/2.

In addition the library includes a number or pre-canned messages and 
has evolved to provide some error related predicates.

---+++ Throwing pack errors

Any term recognised as the first argument of the defined pack_errors:message/3 can be made to spit<br>
a token identifying the originating pack/module and predicate. The main intuition is that this is the<br>
the predicate responsible for the error. You can do this by either wrapping the message or by using<br>
pack_error's own version of throw, throw/2.

Wrapping is via pack_error/2 where the first argument is the message and second is a list of options.

==
?- throw( pack_error(lengths_mismatch(a,b,1,2),[]) ).
ERROR: Lists for a and b have mismatching lengths: 1 and 2 respectively

?- throw( pack_error(lengths_mismatch(a,b,1,2),[foo:bar/1]) ).
ERROR: foo:bar/1: Lists for a and b have mismatching lengths: 1 and 2 respectively
==

You can also use throw/2, which is defined in the pack, without wrapping the Message,

==
?- throw( lengths_mismatch(a,b,1,2), [foo:bar/1] ).
ERROR: foo:bar/1: Lists for a and b have mismatching lengths: 1 and 2 respectively
==

In both cases, you can drop the list if it contains a single element, thus

==
?- throw( lengths_mismatch(a,b,1,2), foo:bar/1 ).
ERROR: foo:bar/1: Lists for a and b have mismatching lengths: 1 and 2 respectively
==

Note that in the latter case (throw/2) the options can also contain terms controling the execution of throw/2.

Options in both cases provide the context:
  * Pname/Arity
     predicate for decoration 
  * Mod:Pname/Arity
     prefixed predicate and decoration
  * pack(Pack)
     pack of the originating predicate
  * pred(Pname/Arity)
     alternative have for identifying the predicate

The library is loosely designed around the principle that most packs will define a homonym module.  If both Pack and Mod 
are given and are the same only one is printed, however if they differ, they will both be shown.
The order of identification is that of going throough the list above from top to bottom.
The first one matching will identify the predicate and stop looking, so alternatives will be ignored.

---+++ Prepacked errors

Argument errors. <br>
Poss is a list of (argument) positions and Args a list of arguments.
  * arg_enumerate(Pos,Vals,Arg)
     see also type/3
  * arg_ground(Pos,Arg), 
     ground argument(s) were expected at Pos
  * arg_ground_in_one_of(Poss,Args)
     at least one ground argument was expected in a list of arguments (Args)
  * arg_ground_pattern(Poss,Args)
     
Printing of Arg(s) itself can be surpressed with prolog_flag(pack_errors_arg,false)- useful for long data.

Other errors 

  * cast(Term,From,To) )
  * lengths_mismatch(Tkn1,Tkn2,Len1,Len2)
  * lengths_mismatch(Tkn1,Tkn2,Op,Len1,Len2)
  * type_error(Type,Term)
  * type_error(Pos,Type,Term)
  * wrong_token(Tkn,Cat)

---+++ Examples

==
?- throw( pack_error(arg_ground(3,name), os:os_ext/3) ).
ERROR: os:os_ext/3: Ground argument expected at position: 3,  (found: name)

?- throw( pack_error(arg_ground(3,name(_)), os:os_ext/3) ).
ERROR: os:os_ext/3: Ground argument expected at position: 3,  (found: name(_4210))

?- set_prolog_flag(pack_errors_arg,true).   % this is the default, so no change in behaviour:

?- throw( pack_error(arg_ground(3,name(_)), os:os_ext/3) ).
ERROR: os:os_ext/3: Ground argument expected at position: 3,  (found: name(_4210))

?- set_prolog_flag(pack_errors_arg,false).

?- throw( pack_error(arg_ground(3,name(_)), os:os_ext/3) ).
ERROR: os:os_ext/3: Ground argument expected at position: 3

?- set_prolog_flag(pack_errors_arg,true).

?- throw( pack_error(arg_enumerate(3,[a,b,c],d), [pack(os),pred(os_pred/3)]) ).
ERROR: os:os_pred/3: Term at position: 3, is not one of: [a,b,c], (found: d)

% use throw/2 as it makes code clearer:
?- throw( arg_enumerate(3,[a,b,c],d), os:os_pred/3 ).
ERROR: os:os_pred/3: Term at position: 3, is not one of: [a,b,c], (found: d)

?- throw( arg_enumerate(3,[a,b,c],d), [pack(os),os_pred/3] ).
ERROR: os:os_pred/3: Term at position: 3, is not one of: [a,b,c], (found: d)

?- throw( lengths_mismatch(a,b,1,2), [pack(foo)] ).
ERROR: foo:_Unk: Lists for a and b have mismatching lengths: 1 and 2 respectively

?- throw( lengths_mismatch(a,b,1,2), pred(bar/1) ).
ERROR: _Unk:bar/1: Lists for a and b have mismatching lengths: 1 and 2 respectively

?- throw( cast(abc('file.csv'),atom), os:os_term/2 ).
ERROR: os:os_term/2: Cannot cast: abc(file.csv), to type: atom
==

Examples from other packs:
==
?- map_list_options( plus_one, In, [2,3,4,5], [add_options(maybe),on_fail(skip)] ).
ERROR: false:map_list_options/4 @ option(add_options): Object of type: boolean, expected but found term: maybe
==

---+++ Defining new pack errors

Example file (available at pack('pack_errors/examples/fold_data_errors.pl')):

==
:- multifile( pack_errors:message/3 ).

pack_errors:message( fold_data_insufficient(Dlen,N) ) -->
    ['Insufficient length of data (~d) as ~d folds are required'-[Dlen,N]].
pack_errors:message( fold_data_residual(Dlen) ) -->
    ['Residual data of length: ~d while splitting folds'-[Dlen]].
==

Load and try with

==
?- [pack('pack_errors/examples/fold_data_errors')].

?- throw( pack_error(fold_data_insufficient(10,20),true) ).
ERROR: Insufficient length of data (10) as 20 folds are required

?- throw( fold_data_insufficient(10,20), mlu:ten_fold/3 ).
ERROR: mlu:ten_fold/3: Insufficient length of data (10) as 20 folds are required
==

---+++ Pack info

The library reacts to =|debug(pack_errors)|= spitting informational message along the execution of library predicates.

Pack predicates:
  * throw/2; =|+Error, +Opts|=
  * caught/3; =|+Goal, +Error, +Opts|=
  * type/2,3; =|+Type, +Term|=
  * ground/2, ground_binary/2; =|+Term, -Groundness|=
  * defined/2,3; =|+Pid,  +From, +Opts|=
  * of_same_length/1; =|+Lists|=
  * of_same_length/2,3; =|+List1, +List2, +Opts|=
  * pack_errors_version/2; =|+Version, +Date|=
  * pack_errors/0

Pack defined errors selection: (see pack('pack_errors/prolog/pack_errors.pl') for a full list)
  * arg_ground(Pos,Arg)
  * arg_ground_in_one_of(Poss,Args) 
  * lengths_mismatch(Tkn1,Tkn2,Len1,Len2)
  * cast(Term,To)

@author  nicos angelopoulos
@version 0.1 2016/01/30
@version 0.2 2016/02/24
@version 0.3 2017/03/06
@version 2.0 2018/10/01
@version 2.1 2019/4/22
@license MIT
@see     http://stoics.org.uk/~nicos/sware/pack_errors

*/

:- multifile prolog:message//1.

caught_defaults( [on_exit(error),on_true(true)] ).

/** caught( +Goal, +Error, +Opts ).

Catches all errors and failure of Goal. The idea is that
all non-successful executions are handled identical by the call.
If Goal errors, the primary thrown ball is caught and discarded. 
If Goal errors or fails, behaviour depends on option value OnExit (see Opts below).

Opts 
  * ball(Ball)
      instantiates the original exception Ball caught from calling Goal.
      (So that parts of it can be included in Error.)

  * on_exit(OnExit=error)
     what to do on failed and errored executions
     * true
        succeeds and repors nothing
     * fail
        reports nothing but call itself fails
     * error
        throws the error (any unrecognised value defaults to error)

  * on_true(OnTrue=true)
     call OnTrue iff Goal was successful (and no handling was done)

==
?- caught( fail, my_exception(on_data), true ).
ERROR: Unhandled exception: pack_error(my_exception(on_data),[on_exit(error),message(error)])

?- caught( fail, my_exception(on_data), on_exit(true) ).
false
% it fails because the message writing fails, which is probably best

?- caught( false,  os_exists_not(abc), [] ).
ERROR: OS entity: abc, does not exist

?- caught( false,  os_exists_not(abc), on_exit(error) ).
ERROR: OS entity: abc, does not exist

?- caught( false,  os_exists_not(abc), on_exit(fail) ).
ERROR: OS entity: abc, does not exist
false.

?- caught( false,  os_exists_not(abc), on_exit(true) ).
ERROR: OS entity: abc, does not exist
true.
==

@see throw/2 

*/
caught( Goal, Error, Args ) :-
    pack_errors_options_append( caught, Args, Opts ),
    caught_opts( Goal, Error, Opts ).

caught_opts( Goal, Error, Opts ) :-
    catch( Goal, Ball, pack_errors:caught_error(Goal,Ball,Error,Opts) ),
    memberchk( OnTrue, Opts ),
    call( OnTrue ),
    !.
caught_opts( _Goal, Error, Args ) :-
    pack_errors_options_append( caught, Args, Opts ),
    caught_opt_throw( OnThrow, Opts ),
    throw( Error, on_exit(OnThrow) ).

caught_error( _Goal, Ball, Error, Opts ) :-
    memberchk( ball(Ball), Opts ),
    !,
    caught_opt_throw( OnThrow, Opts ),
    throw( Error, on_exit(OnThrow) ).
caught_error( _Goal, _Ball, Error, Opts ) :-
    caught_opt_throw( OnThrow, Opts ),
    throw( Error, on_exit(OnThrow) ).

caught_opt_throw( OnThrow, Opts ) :-
    memberchk( on_exit(Rep), Opts ),
    caught_opt_report_throw( Rep, OnThrow ).

caught_opt_report_throw( true, true ) :-
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

throw_defaults([err(error)]).

% fixme: use _known and throw error else
throw_err_opt_vals( error, error, error ).
throw_err_opt_vals( test, quiet, false ).
throw_err_opt_vals( exists, warning, false ).

/** throw( +Error, +Opts ).

An optionised version of throw/1. The Error is not  always thrown (eg OnThrow==false, see Opts below).<br>
This version of throw() decouples type of message printing and execution behaviour.<br>

As of version 0.3 this should be the adviced entry point for message writing and ball throwing
for stoics packs.

Opts
  * err(Err=error)
      convenenience option that sets both Level and OnExit, if they are absent
      * error
         =|Level = error|= and =|OnExit = error|=

      * test
          =|Level=quiet|= and =|OnExit = false|=

      * exists
          =|Level=warning|= and =|OnExit = false|=

  * on_exit(OnExit=error)
     defines execution behaviour on exiting the printing of the error. One
     of =|[true,false,error]|=. if not given the default depends on Err,
     * true
        succeed
     * false
        fails
     * error
        errors

  * Pid
     predicate indicator (=|foo:bar/1|= or =|bar/2|=)

  * pack(Pack)
     originator pack/module
 
  * pred(Pid)
     originator predicate

  * option(Opt)
     name of originator option

  * message(Level=error)
     passed to print_message/2 (first argument), but also accepts quiet (as silent still prints things...)

==
?- 
    throw( cast(abc('file.csv'),atom) ).

ERROR: Unhandled exception: cast(abc(file.csv),atom)

?- 
    throw( pack_error(cast(abc('file.csv'),atom),true) ).

ERROR: Cannot cast: abc(file.csv), to type: atom

?-
    Opt = os:os_exists/2,
    throw(pack_error(cast(abc('file.csv'),atom),Opt)), writeln(later).

ERROR: os:os_exists/2: Cannot cast: abc(file.csv), to type: atom

?- 
    throw(cast(abc('file.csv'),atom), os:os_exists/2), writeln(later).

ERROR: os:os_exists/2: Cannot cast: abc(file.csv), to type: atom

?- 
    throw(cast(abc('file.csv'),atom), err(test)), writeln(later).

false.

?- 
    _Opts = [message(quiet),on_exit(true)],
    throw(cast(abc('file.csv'),atom), _Opts), writeln(later).

later
true.

?- 
    _Opts = [message(warning),on_exit(true)],
    throw(cast(abc('file.csv'),atom), _Opts), writeln(later).

Warning: Cannot cast: abc(file.csv), to type: atom
later
true.

?- 
    _Opts = [message(informational),on_exit(true)],
    throw(cast(abc('file.csv'),atom), _Opts), writeln(later).

% Cannot cast: abc(file.csv), to type: atom
later
true.

?- 
   _Opts = [message(warning),on_exit(false)],
   throw(cast(abc('file.csv'),atom), _Opts), writeln(later).

Warning: Cannot cast: abc(file.csv), to type: atom
false.

?- 
    throw(cast(abc('file.csv'),atom), err(exists)), writeln(later).

Warning: Cannot cast: abc(file.csv), to type: atom
false.

?- 
    throw(cast(abc('file.csv'),atom), on_exit(true)), writeln(later).

ERROR: Cannot cast: abc(file.csv), to type: atom
later
true.

?- 
    throw(cast(abc('file.csv'),atom), on_exit(false)), writeln(later).

ERROR: Cannot cast: abc(file.csv), to type: atom
false.

?-
    throw(cast(abc('file.csv'),atom), on_exit(error)), writeln(later).

ERROR: Cannot cast: abc(file.csv), to type: atom

?- 
    throw(cast(abc('file.csv'),atom), message(warning)), writeln(later).

Warning: Cannot cast: abc(file.csv), to type: atom

?- 
    throw(cast(abc('file.csv'),atom), message(informational)), writeln(later).

% Cannot cast: abc(file.csv), to type: atom
later
true.

?-  
    _Opts = [message(informational),on_exit(false)],
    throw(cast(abc('file.csv'),atom), _Opts), writeln(later).

% Cannot cast: abc(file.csv), to type: atom
false.

?- 
    _Opts = [message(informational),on_exit(error)],
    throw(cast(abc('file.csv'),atom), _Opts), writeln(later).

% Cannot cast: abc(file.csv), to type: atom

==

@author nicos angelopoulos
@version  0.2 2017/3/6
@version  0.3 2018/1/5   added tracer options: pack, pred & pack_format
@version  0.4 2018/9/30  severe re-write, see docs

*/
throw( Error, Args ) :-
    pack_errors_options_append( throw, Args, Opts ),
    memberchk( err(Err), Opts ),
    throw_err_opt_vals( Err, LvlDef, OnXDef ),
    ( memberchk(message(Lvl),Opts)-> true; Lvl = LvlDef ),
    ( memberchk(on_exit(OnExit),Opts) -> true ; OnExit = OnXDef ),
    throw_on_valid( OnExit ),
    throw_level( Lvl, Error, OnExit, Opts ).

throw_on_valid( OnThrow ) :-
    throw_on_known( OnThrow ),
    !.
throw_on_valid( OnThrow ) :-
    % fixme: render it !
    throw( unknown_option_value(throw/2,on_exit(OnThrow)) ).

throw_on_known(error).
throw_on_known(true).
throw_on_known(fail).
throw_on_known(false).

% fixme: ask in forum with silent in print_message/2 prints things ...
%
throw_level( quiet, _BallMark, OnExit, _Opts ) :-
    !,
    throw_level_exit( OnExit ).
throw_level( Lvl, BallMark, OnExit, Opts ) :-
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
    throw_level_on_exit( OnExit, Lvl, Ball ).

throw_level_on_exit( error, error, Ball ) :-
    !,
    throw( Ball ).
throw_level_on_exit( OnExit, Lvl, Ball ) :-
    debug( pack_errors, 'Explicit layout at level: ~w', [Lvl] ),
    prolog:message( Ball, Mess, [] ), % i thinks [] is correct
	print_message_lines( current_output, kind(Lvl), Mess ),
    throw_level_exit( OnExit ).

throw_level_exit( error ) :-
    !,
    throw( true ).
    % abort.
throw_level_exit( Goal ) :-
    call( Goal ),
    !.

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
    % throw( pack_error(pack_error,type/3,arg_ground(1,Type)) ).
    throw( arg_ground(1,Type), pack_error:type/3 ).
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
    % memberchk( pack(Pack), Opts ),
    % memberchk( pred(Pred), Opts ),
    memberchk( arg(Pos), Opts ),
    type_error_position( Pos, Type, Term, Opts ).

% type_error_position( false, Pack, Pred, Type, Term ) :-
type_error_position( false, Type, Term, Opts ) :-
    !,
    throw( pack_error(type_error(Type,Term)), Opts ). 
type_error_position( Pos, Type, Term, Opts ) :-
    throw( pack_error(type_error(Pos,Type,Term)), Opts ). 

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
?- defined( b_real/0, lib(b_real), load(true) ).
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
      name of List1 
      (used in error throwing to id the list)
  * token2(Tkn2=n)
      name of List2 (used in error throwing) 
      when first argument is Lists, Tkn2 will be the index position of the first list length-mismatch the head list

==
?- of_same_length( [a,b,c], [1,2,3] ).
true.

?- of_same_length( [[a,b,c],[1,2,3]] ).
true.

?- of_same_length( [1,2,3], [a,b], token1(first) ).
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
of_same_length_mismatch( false, _Lng1, _Lng2, _Tkn1, _Tkn2, _Opts ) :-
    fail.
% throw_lists ? which will also include the offender & base lists ?
of_same_length_mismatch( throw, Lng1, Lng2, Tkn1, Tkn2, _Opts ) :-
    throw( of_same_length(Lng1,Lng2,Tkn1,Tkn2) ).
of_same_length_mismatch( warning, Lng1, Lng2, Tkn1, Tkn2, _Opts ) :-
    % % Format = 'Lists at:~w and ~w, have differing lengths: ~d and ~d',
    % % message_report( Format, [Tkn1,Tkn2,Lng1,Lng2], informational ).
    throw( lengths_mismatch(Tkn1,Tkn2,Lng1,Lng2), message(warning) ).
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
    nth1( N, Opts, Mod:Pred, Rest ),
    \+ (nth1(N1,Opts,_Name/_Arity), N1<N),
    !,
    pack_message_options_trail( Rest, Trail, Rems ),
    Apts = [pred(Mod:Pred),trail(Trail)|Rems]. 
pack_message_options_augment( Opts, Apts ) :-
    nth1( _N1, Opts, Name/Arity, Rest ),
    !,
    pack_message_options_trail( Rest, Trail, Rems ),
    Apts = [pred(Name/Arity),trail(Trail)|Rems].
    %  fixme: just stick a mod infront of Name/Arity
pack_message_options_augment( Apts, Apts ).

pack_message_options_trail( [], [], [] ).
pack_message_options_trail( [Opt|Opts], Trail, Rems ) :-
    ( (Opt=_Mod:_Name1/_Arity1; Opt=_Name2/_Arity2) ->
        Trail = [Opt|TellTail],
        Rems = Tems
        ;
        Trail = TellTail,
        Rems = [Opt|Tems]
    ),
    pack_message_options_trail( Opts, TellTail, Tems ).

/* fixme: delete
pack_message_options_augment( Opts, Apts ) :-
    nth1( N, Opts, Name/Arity, Rest ),
    pack_message_options_trail( Rest, Trail, Rems ),
    ( select(Mod:Pred,Opts,Rem) ->
        Apts = [pred(Mod:Pred)|Rem]
        ;
        ( select(Name/Arity,Opts,Rem) ->
            Apts = [pred(Name/Arity)|Rem]
            ;
            Apts = Opts
        )
    ).
*/

/** pack_errors_version( -Version, -Date ).

Current version and release date for the library.

==
V = 2:1:0,
D = date(2019, 4, 22).
==
@author nicos angelopoulos
@version  2:1 2019/4/22
*/
pack_errors_version( 2:1:0, date(2019,4,22) ).

prolog:message(unhandled_exception(true)) --> [].
prolog:message(unhandled_exception(pack_error(Message))) -->
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

pack_message( Mess, OptsPrv ) -->
    % fixme: check for var(OptsPrv) ?
    {( is_list(OptsPrv) -> OptsPrv = Opts; Opts = [OptsPrv] )},
    {pack_message_options_augment(Opts,Apts)},
    message_pack( Apts ),
    pack_errors:message( Mess ),
    pack_message_trail( Apts ).

pack_message_trail( Apts ) -->
    { memberchk(trail(Trail),Apts),
      Trail \== [],
      !
    },
    [ '\nERROR: Trail: ~w' - [Trail] ].
pack_message_trail( _Apts ) --> {true}.

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
    {( memberchk(option(OptNm),Opts) -> atomic_list_concat([' @ option(',OptNm,')'],OptTkn); OptTkn = '' )},
    ['~w:~w~w: '-[Pack,Pred,OptTkn] ].
message_pack( _ )  --> [].

:- multifile( pack_errors:message/3 ).

message( true ) --> [].

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
message( wrong_token(Tkn,Cat) ) -->  % was: unknown_token/2
    ['Token: ~w, is not a recognisable: ~w'-[Tkn,Cat]].
message( expected_from(_,Pid,false) ) -->
    ['Predicate: ~w is not defined'-[Pid]].
message( expected_from(false,Pid,From) ) -->
    ['Predicate: ~w is not defined (source apparently available at: ~w; not asked to load)'-[Pid,From]].
message( expected_from(true,Pid,From) ) -->
    ['Predicate: ~w is not defined (source apparently available at: ~w; which was loaded).'-[Pid,From]].
message( true ) -->
    [].
