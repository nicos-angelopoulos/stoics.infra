:- module( debug_call, 
        [ 
            debug_call/2,
            debug_call/3,
            debug_call/4,
            debug_chain/2, debug_chain/3,
            debug_consec/3, debug_consec/4,
            debug_message/3,
            debug_on/1,
            debug_portray/2,
            debug_set/2,
            debugging_status/2,
            debug_topic/2,
            debug_topic/3,
            debug_call_version/2,
            debugging_topic/1
       ] ).

:- multifile(user:message_property/2).
:- dynamic(debug_call_message_property/2).

user:message_property( Dbg, Property ) :-
    debug_call_message_property( Dbg, Property ).

/** <module> Debugging with calls.

Avoids running goals to produce output that is only
relevant while debugging. Includes pre-canned, often used calls.

---+++ Examples

==

?- debug( ex ).
?- debug_call( ex, length, '', list1/[x,y,z] ).
% Length for list, list1: 3

?- debug_call( ex, length, 'some prefix', [list1,list2]/[[x,y,z],[a,b,c]] ).
% some prefix lengths for lists, list1: 3, list2: 3

?- debug_call( ex, dims, [m1,m2]/[[a(x),a(y),a(z)],[xy(a,b),xy(c,d),xy(e,f)]] ).
%  Dimensions for matrices,  (m1) nR: 3, nC: 1. (m2) nR: 3, nC: 2.

?- debug_call( ex, wrote, loc(file,csv) ).
% Could not locate wrote on file specified by: file, and extensions: csv
?- csv_write_file( 'file.csv', [] ).

?- debug_call( ex, wrote, loc(file,csv) ).
% Wrote on file: '/home/nicos/pl/lib/src/trace/file.csv'

?- debug_call( ex, task(stop), 'write on file' ).
At 15:44:1 on 2nd of Jul 2014 finished task: write on file.

?- assert( (simple_mess(KVs,Mess,Args):- KVs =[a=A,b=B], atom_concat(A,B,Mess), Args=[]) ).
?- debug_call( ex, simple_mess([a=1,b=2],

==

---+++ Variable topics 

This library avoids the messy way in which package(debug) deals with variable debug topics. 
That is, their term expansion and subsequent pattern matching mishandles goals of the form
debugging/1 and debug/3 that have an unbound variable in the 1st argument.
debug_calls uses dynamic -.. 


---+++ Pack info 

@author nicos angelopoulos
@see http://stoics.org.uk/~nicos/sware/debug_call/
@tbd options_debug( Opts, Mess, Args )  only writes if Opts contains debug(true). maybe this should be part of pack(options)
@tbd provide a way to remove lib(debug)'s expansions
@version 0.1 2016/3/5
@version 0.2 2016/11/01
@version 0.3 2017/3/9
@version 1.1 2018/3/20
@version 1.2 2019/4/22

*/

/** debug_call_version( -Version, -Date ).

Current version and release date for the library.

==
?- debug_call_version( V, D ).
V = 1:2:0,
D = date(2019,4,22).
==
*/
debug_call_version( 1:2:0, date(2019,4,22) ).

:- use_module(library(lib)).

:- lib(source(debug_call), [homonyms(true),index(false)]).
:- lib(stoics_lib:locate/3 ).
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:message_report/3).
:- lib(stoics_lib:datime_readable/1).
:- lib(end(debug_call) ).

%% debug_call( +Topic, +Goal ).
%
%  Only call debug if we are debugging Topic.
%
%  If Goal with arity +2 is available call that instead of Goal with extra arguemnts Mess and Args
%  that will be passed to debug/3. If the goal (original or +2) fail, nothing is printed by
%  debug_call and the debug_call(T,G) itself succeeds.
% 
%==
% ?- goal( Goal, Mess, Args ).
%==
% 
% Examples
%==
% ?- assert( (simple_mess(KVs,Mess,Args):- KVs =[a=A,b=B], atom_concat(A,B,Mess), Args=[]) ).
% ?- debug_call( ex, simple_mess([a=1,b=2], 
%==
%
% @author nicos angelopoulos
% @version  0.2 2018/3/20
%
debug_call( Topic, Goal ) :-
    debugging_topic( Topic ),
    !,
    debug_call_goal( Topic, Goal ).
debug_call( _Topic, _Goal ).

debug_call_goal( Topic, Moal ) :-
    ( Moal = Mod:Goal -> true; Goal = Moal, Mod=user ),
    functor( Goal, Functor, Arity ),
    Extra is Arity + 2,
    current_predicate( Mod:Functor/Extra ),
    !,
    ( call(Mod:Goal,Mess,Args) ->
        debug( Topic, Mess, Args )
        ;
        true
    ).
debug_call_goal( _Topic, Goal ) :-
    ( call(Goal) -> true; true ).

%% debug_chain( +TopicCond, +TopicDep ).
%% debug_chain( +TopicCond, +TopicDep, -TDprior ).
%
% If already debugging TopicCond, then also start debugging TopicDep ).
% TDprior is true if TopicDep was already debugging, else is false.
% Current implementation sets TDprior to true whenever Topic is not
% debugged, as it assumes that this value best suit independent fluctuation
% of TopicDep. Only in the case of debug_chain/2, TopicDep can be a list.
%
% @author nicos angelopoulos
% @version  0.1 2014/4/4
% @version  0.2 2016/11/1
% @see debug_set/2
%
debug_chain( Topic, Then ) :-
    to_list( Then, Thens ),
    maplist( debug_chain(Topic), Thens, _Priors ).

debug_chain( Topic, Then, Prior ) :-
    debugging_topic( Topic ),
    !,
    debugging_status( Then, Prior ),
    debug( Then ).
debug_chain( _Topic, _Then, true ). 
    % setting 3rd to true is a bit presumptious of its uses later on

/** debug_message( +Topic, +Mess, +Args ).

A wrap around debug/3 that calls it by constructing the term on-the-fly. 
So that lib(debug) does not create a record by inspecting the term (via expansion).
Particularly useful in sending uninstantiated Topics.

==

==

@author nicos angelopoulos
@version  0.1 2016/11/1

*/
debug_message( Topic, Mess, Args ) :-
    Call =.. [debug,Topic,Mess,Args],
    call( Call ).

/** debugging_topic( ?Topic ).

A wrap around debugging/1 that calls it by constructing the term on-the-fly. 
So that lib(debug) does not create a record by inspecting the term (via expansion).
Particularly useful in sending uninstantiated Topics.

==

==

@author nicos angelopoulos
@version  0.1 2016/11/1

*/
debugging_topic( Topic ) :-
    Call =.. [debugging,Topic],
    call( Call ).

%% debugging_status( +Topic, -Status ).
%
% Status == true iff debugging(Topic) succeeds. Else, it is false.
% Similar to debugging/2, but does not fail for undefined Topic.
%==
% ?- debug( something ).
% true.
% ?- debugging_status( something, Some ).
% Some = true.
% ?- debugging_status( some_else, Else ).
% Else = false.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/23
debugging_status( Topic, Status ) :-
    debugging_topic( Topic ),
    !,
    Status = true.
debugging_status( _Topic, false ).

%% debug_set( +Prior, +Topic ).
%
% Reset Topic according to Prior: true sets Topic to on and false turns Topic off.
%
%==
% ?- nodebug( chained ).
% true.
% ?- debug( testo ).
% Warning: testo: no matching debug topic (yet)
% true.
% ?- debug( chained, 'debugs chains 1', [] ).
% true.
% ?- debug_chain( testo, chained, Prior ).
% Prior = false.
% ?- debug( chained, 'debugs chains 2', [] ).
% % debugs chains 2
% true.
% ?- Prior = false, debug_set( Prior, chained ).
% Prior = false.
% ?- debug( chained, 'debugs chains 3', [] ).
% true
%==
% @author nicos angelopoulos
% @version  0.1 2014/7/23
% @version  0.2 2016/8/22, Prior == true used to do nothing, now it turns topic on. also renmaed from debug_set/2.
% @see debug_chain/3
%
debug_set( false, Topic ) :-
    nodebug( Topic ).
debug_set( true, Topic ) :-
    debug( Topic ).

/** debug_topic( +Topic, +Opts, -Restore ).

Start debugging Topic if options(debug(true),Opts), with Restore
being instantiated to a term that can be used to restore the 
original debug state of Topic (see options_restore/2). If options(debug(false),Opts)
then Topic is stopped from being debugged (Restore still holds the 
correct term for restoring debugging state for topic to precall status).

==
?- assert( ( on_t(I,Topic) :- (debugging(Topic) -> write(I-y(Topic)) ; write(I-n(Topic))), nl ) ).
?- T = options, debug(T), on_t(1,T), debug_topic(T,[debug(false)],R), on_t(2,T), debug_set(R,T), on_t(3,T).
1-y(options)
2-n(options)
3-y(options)
T = options,
R = true.

?- T = options, nodebug(T), on_t(1,T), debug_topic(T,[debug(true)],R), on_t(2,T), debug_set(R,T), on_t(3,T).
1-n(options)
2-y(options)
3-n(options)
T = options,
R = false.
==

@author nicos angelopoulos
@version  0.1 2016/8/22

*/
debug_topic( Topic, Opts, Restore ) :-
    memberchk( debug(Dbg), Opts ),
    Dbg == true,
    !,
    debug_topic_restore( Topic, Restore ),
    debug( Topic ).
debug_topic( Topic, _Opts, Restore )  :-        % becomes default under this implementation
    debug_topic_restore( Topic, Restore ),
    nodebug( Topic ).

debug_topic_restore( Topic, Restore ) :- 
    debugging_topic( Topic ),
    !,
    Restore = true.
debug_topic_restore( _Topic, false ).

%% debug_topic( +Flag, +Topic ).
%
% Start debugging Topic if Flag == true, and stop debugging if Flag == false.
% 
%==
% ?- debug_topic( true, example ).
%==
% @author nicos angelopoulos
% @version  0.1 2014/12/10
% @version  0.2 2016/08/22, added nodebug/1 when Flag == false
% @see options_append/4
%
debug_topic( true, Topic ) :-
    debug( Topic ).
debug_topic( false, Topic ) :-
    nodebug(Topic).

%% debug_on( +Topic ).
%
% As debug/1, but do not print warning if topic is not known.
%
debug_on( Topic ) :-
    asserta( prolog_debug:debugging(Topic,true,[user_error])).

%% debug_portray( +Topic, +Term ).
%
%  Call portray_clause(Term) if we are debugging Topic.
%
% @author nicos angelopoulos
% @version  0.1
%
debug_portray( Topic, Term ) :-
    debugging_topic( Topic ),
    !,
    portray_clause( Term ).
debug_portray( _Topic, _Term ).

%% debug_call( +Topic, +Goal, +Arg ).
%% debug_call( +Topic, +Goal, +Mess, +Arg ).
%
%  Automates often used debug calls. When Pfx is missing it is assumed to be ''. It can also be used 
%  to call arbitrary Goal and then print a message after it has successfull completed.
%  
%  When Goal is a known abbreviation, then Arg usually qualifies the output generated.
%  When Goal is of the form call(Goal), Arg will be passed to debug(Topic,Mess,Arg). 
% 
%Goal in:
%  * call(Goal)
%    call Goal before printing debugging message debug( Topic, Mess, Args).  (Goal is called in non-deterministic context).
%  * dims
%    prints the dimensions of matrix, see mtx_dims/3
%  * end
%    translates to finishing ~Arg or starting ~Topic if Arg == true
%  * goal
%    anything that does n't match any of the above is retried as call(Goal)
%  * length
%    prints the lenghts of a bunch of lists. Args should be ListNames/Lists. 
%    uses non list ListNames if debuging the length of a single list, in which case
%    message in the singular is used.
%  * list
%    writes contents of list with header and footer. Arg should be of the form Hdr/Ftr/List, 
%    else it is translated as Hdr/''/List or ''/''/List. 
%    If Hdr or Ftr are '' then that part of the message is skipped
%  * ns_sel
%    first argument is the item selected from second arg list (only reported if 2nd arg is not a singleton (ns))
%    accepts 2 optional args, 3rd is the token of what is selected (false for printing nothing on the subject, default)
%    and 4th is whether to report if the 2nd argument is indeed a singleton (default: false)
%  * ns_sel(true)
%    first argument is the item selected from second arg list. reports differently if 2nd arg is a singleton, but always does report
%  * odir
%    output directory (Arg should exist and be a directory)
%  * pwd 
%    message about the current directory location (if Arg == false, it is ignored)
%  * read
%    reports reading from a file. Arg should be file specification suitable for locate/3.
%    Either loc(File,Exts) or simply File in which case Exts = ''.
%  * start 
%    translates to starting ~Arg or starting ~Topic if Arg == true
%  * task(Wch)  
%    time of start/stop of a task. Other values are allowed put printed as is. 
%  * term
%    simply spew the input term
%  * var
%    reports variable name (arg(1)) and its current instantiation (arg(2))
%  * wrote 
%    reports the writting of output on a file. Arg should be file specification suitable for locate/3.
%    Either loc(File,Exts) or simply File in which case Exts = ''.
%
%==
% ?- debug( ex ).
% ?- debug_call( ex, length, '', list1/[x,y,z] ).
% % Length for list, list1: 3
%  
% ?- debug_call( ex, length, 'some prefix', [list1,list2]/[[x,y,z],[a,b,c]] ).
% % some prefix lengths for lists, list1: 3, list2: 3
% 
%?- debug_call( ex, wrote, loc(file,csv) ).
%% Could not locate wrote on file specified by: file, and extensions: csv
%?- csv_write_file( 'file.csv', [] ).
%
%?- debug_call( ex, wrote, loc(file,csv) ).
%% Wrote on file: '/home/nicos/pl/lib/src/trace/file.csv'
%
%?- debug_call( ex, task(stop), 'write on file' ).
%% At 15:44:1 on 2nd of Jul 2014 finished task: write on file.
%    
%?- debug_call( ex, (length([a,b,c],L),write(len(L)),nl) ).
%len(3)
%L = 3.
%
%?-  Etcs = [suv-17.09.26.txg,suv-17.09.21.txg], Etc = suv-17.09.26.txg,
%    debug_call( suv, ns_sel, c(Etc,Etcs,'suv file',true) )
% Continuing with: suv file, as: suv-17.09.26.txg, from non singleton list: [suv-17.09.26.txg,suv-17.09.21.txg]
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/03/27
% @version  0.2 2014/04/24  added wrote
% @version  0.3 2014/07/2   added task
% @version  0.4 2014/09/22  renamed from debug_call/3
% @version  0.5 current     added ns_sel
% @version  1.1 2018/3/20   prefer +2 arity in debug_call/2
%
debug_call( Topic, Goal, Args ) :-
    debug_call( Topic, Goal, '', Args ).

debug_call( Topic, Goal, Mess, Args ) :-
    debugging_topic( Topic ),
    !,
    debugging_call( Topic, Goal, Mess, Args ).
debug_call( _Topic, _Goal, _Mess, _Args ).

debugging_call( Topic, Goal, Mess, Args ) :-
    debug_call_topic( Goal, Mess, Args, Topic ),
    !.
debugging_call( Topic, call(Goal), Mess, Args ) :-
    !,
    call( Goal ),
    debug_message( Topic, Mess, Args ).
debugging_call( Topic, Goal, Mess, Args ) :-
    call( Goal ),
    debug_message( Topic, Mess, Args ).

debugging_call( Topic, Goal, Mess, Args ) :-
    Called = debug_call(Topic,Goal,Mess,Args),
    message_report( 'failure ignored on: ~w', Called, warning ).

%% debug_consec( +Topic, +Mess, +Args ).
%% debug_consec( +Topic, +Clrs, +Mess, +Args ).
% 
% Alternate the colours of printing messages on Topic,
% from those in Clrs. When missing these are [blue,magenta]. 
% As of v0.2 Clrs can be a single colour.
% 
%==
% ?- debug( dbg ).
% ?- debug_consec( dbg, 'what:~w', when ).
% % what: when            <- in blue
%
% ?- debug_consec( dbg, 'what:~w', when ).
% % what: when            <- in magenta
%
% ?- debug_consec( dbg, [blue,green], 'what:~w', when ).
% % what: when            <- in blue
%
% ?- debug_consec( dbg, [blue,green], 'what:~w', when ).
% % what: when            <- in green
% 
%==
% 
% Version 0.2
%==
% ?- debug_consec( dbg, magenta, 'what:~w', when ).
% % what: when            <- in magenta
%==
%
% @author nicos angelopoulos
% @version  0.2 2019/12/29
% @version  0.1 2014/7/24
%
debug_consec( Topic, Mess, Args ) :-
    Clrs = [blue,magenta],
    debug_consec( Topic, Clrs, Mess, Args ).

debug_consec( Topic, ClrS, Mess, Args ) :-
    debugging_topic( Topic ),
    !,
    ( is_list(ClrS) -> Clrs = ClrS; Clrs = [ClrS] ),
    debug_consec_topic( Topic, Clrs, Mess, Args ).
debug_consec( _Topic, _Clrs, _Mess, _Args ).

debug_consec_topic( Topic, Clrs, Mess, Args ) :-
    with_output_to( atom(Topicat), write_term(Topic,[]) ),
    ( nb_current(Topicat,Value) -> true; Value = 1 ),
    ( nth1(Value, Clrs, Clr) -> true; Clrs = [Clr|_] ),
    debug_consec_color( Topic, Clr, Mess, Args ),
    length( Clrs, Len ),
    ( Value < Len -> Next is Value + 1; Next is 1 ),
    nb_setval( Topicat, Next ).

debug_consec_color( Topic, Clr, Mess, Args ) :-
    user:message_property( debug(_), color(Attrs) ),
    !,
    retractall( debug_call_message_property(debug(_),color(_)) ),
    assert( debug_call_message_property(debug(_),color(fg(Clr))) ),
    debug_message( Topic, Mess, Args ),
    retractall( debug_call_message_property(debug(_),color(_)) ),
    assert( debug_call_message_property(debug(_),color(Attrs)) ).
debug_consec_color( Topic, Clr, Mess, Args ) :-
    assert( debug_call_message_property(debug(_),color(fg(Clr))) ),
    debug_message( Topic, Mess, Args ),
    retractall( debug_call_message_property(debug(_),color(_)) ).

debug_call_topic( length, Pfx, NamesPrv/ListsPrv, Topic ) :-
                            % add version without names
    ( is_list(NamesPrv) -> Names=NamesPrv, ListsPrv=Lists, With = 'Lengths for lists, '
                           ; [NamesPrv] = Names, [ListsPrv]=Lists, With = 'Length for list, ' 
    ),
    debug_message_prefixed( Pfx, With, Prefixed ),
    maplist( length, Lists, Lengths ),
    findall( ['~w: ~w',', '], member(_,Lengths), WsNest ),
    flatten( WsNest, WsL ),
    once( append(WsLComma,[_],WsL) ),
    append( WsLComma, ['.'], WsLDot ),
    atomic_list_concat( WsLDot, '', Right ),
    atom_concat( Prefixed, Right, Message ),
    findall( [Name,Length], (nth1(N,Names,Name),nth1(N,Lengths,Length)), NLNest ),
    flatten( NLNest, NLs ),
    debug_message( Topic, Message, NLs ). % do the messaging
debug_call_topic( list, _Pfx, InArg, Topic ) :-
    ground( InArg ),
    ( InArg = Left/List -> 
        ( Left = Hdr/Ftr -> true ; Hdr = Left, Ftr = '' )
        ;
        List = InArg, Hdr = '', Ftr = ''
    ),
    debug_call_topic_list_delim( Hdr, Topic, Pfx, 'Starting enumeration of list: ~w' ),
    maplist( debug_message(Topic,'~w'), List ),
    debug_call_topic_list_delim( Ftr, Topic, Pfx, 'Ended enumeration of list: ~w' ).
debug_call_topic( dims, Pfx, NamesPrv/MtxsPrv, Topic ) :-
    ( is_list(NamesPrv) -> Names=NamesPrv, MtxsPrv=Mtxs, With = 'Dimensions for matrices, '
                           ; [NamesPrv] = Names, [MtxsPrv]=Mtxs, With = 'Dimensions for matrix, ' 
    ),
    debug_message_prefixed( Pfx, With, Prefixed ),
    maplist( debug_mtx_dims, Mtxs, NRows, NCols ),

    findall( PartM, (member(_,Names),PartM=' (~w) nR: ~d, nC: ~d.'), MParts ),
    atomic_list_concat( MParts, '', Right ),
    findall( [Name,NRow,NCol], (nth1(N,Names,Name),nth1(N,NRows,NRow),nth1(N,NCols,NCol)), NNest ),
    flatten( NNest, Vars ),
    atom_concat( Prefixed, Right, Message ),
    debug_message( Topic, Message, Vars ). % do the messaging !
debug_call_topic( var, Pfx, DbgTerm, Topic ) :-
    arg( 1, DbgTerm, Var ),
    arg( 2, DbgTerm, Val ),
    Mess = 'Variable: ~a, value: ~w',
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, [Var,Val] ).
debug_call_topic( term, Pfx, DbgTerm, Topic ) :-
    Mess = '~w',
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, [DbgTerm] ).
debug_call_topic( odir, Pfx, Odir, Topic ) :-
    ( exists_directory(Odir) ->
        Mess = 'Ouput in directory: ~w'
        ;
        Mess = 'Output (claimed) in (non-existing) directory: ~w'
    ),
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, [Odir] ).
debug_call_topic( wrote, Pfx, ForLoc, Topic ) :-
    ( ForLoc = loc(Spec,Ext) -> true; Spec=ForLoc, Ext = '' ),
    catch( locate(Spec,Ext,Loc), Excp, true ),
    MessW = 'Wrote on file: ~p',
    debug_call_location_exception_message( Excp, Loc, MessW, Mess, Args ),
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, Args ).
debug_call_topic( read, Pfx, ForLoc, Topic ) :-
    ( ForLoc = loc(Spec,Ext) -> true; Spec=ForLoc, Ext = '' ),
    catch( locate(Spec,Ext,Loc), Excp, true ),
    MessW = 'Read from file: ~p',
    debug_call_location_exception_message( Excp, Loc, MessW, Mess, Args ),
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, Args ).
debug_call_topic( task(Whc), Pfx, Task, Topic ) :-
    datime_readable( Readable ),
    debug_call_topic_time_which_readable( Whc, Whcable ),
    atomic_list_concat( [Readable,' ',Whcable,' task: ', Task,'.'], Mess ),
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, [] ).
debug_call_topic( start, Pfx, Arg, Topic ) :-
    Mess = 'Starting: ~w',
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    ( Arg == true -> Rep = Topic; Rep = Arg ),
    debug_message( Topic, Prefixed, [Rep] ).
debug_call_topic( end, Pfx, Arg, Topic ) :-
    Mess = 'Finished: ~w',
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    ( Arg == true -> Rep = Topic; Rep = Arg ),
    debug_message( Topic, Prefixed, [Rep] ).
debug_call_topic( pwd, Pfx, Stage, Topic ) :-
    working_directory( Pwd, Pwd ),
    ( Stage == false -> 
        Mess = 'Pwd: ~p', Args = [Pwd]
        ;
        Mess = 'Pwd at, ~w, is: ~p', Args = [Stage,Pwd]
    ),
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, Args ).
debug_call_topic( ns_sel, Pfx, Term, Topic ) :-
    % ( Term = [Fst,Sec] -> true; arg(1,Term,Fst),arg(2,Term,Sec) ),
    arg( 1, Term, Fst ), 
    arg( 2, Term, Sec ),
    functor( Term, _Tname, Arity ),
    ( Sec == [] -> 
        true % fixme: it will make more sense to throw an error if Sec = []
        ;
        ( Sec = [_Single] ->
            ( (Arity>3,arg(4,Term,true)) ->
                ( (Arity>2,\+ arg(3,Term,false)) ->
                    arg(3,Term,Trd),
                    Mess= 'Continuing with: ~w as: ~w, (only match).', MArgs = [Trd,Fst]
                    ;
                    Mess= 'Continuing with only match: ~w.', MArgs = [Fst,Sec]
                )
                ;
                Mess = 'Continuing: ~w, from non singleton list: ~w', MArgs = [Fst,Sec]
            )
            ;
            ( (Arity>2,\+ arg(3,Term,false)) ->
                arg(3,Term,Trd),
                Mess = 'Continuing with: ~w, as: ~w, from non singleton list: ~w', MArgs = [Trd,Fst,Sec]
                ;
                Mess = 'Continuing: ~w, from non singleton list: ~w', MArgs = [Fst,Sec]
            )
        ),
        debug_message_prefixed( Pfx, Mess, Prefixed ),
        debug_message( Topic, Prefixed, MArgs )
    ).

debug_call_topic_list_delim( '', _Topic, _Pfx, _Mess ).
debug_call_topic_list_delim( ListName, Topic, Pfx, Mess ) :-
    debug_message_prefixed( Pfx, Mess, Prefixed ),
    debug_message( Topic, Prefixed, [ListName] ).

debug_call_topic_time_which_readable( Wch, Wchable ) :-
    debug_call_topic_time_which_readable_known( Wch, Wchable ),
    !.
debug_call_topic_time_which_readable( Wch, Wch ).

debug_call_topic_time_which_readable_known( start, starting ).
debug_call_topic_time_which_readable_known( finish, finished ).

debug_call_location_exception_message( Var, Loc, MessI, MessO, Args ) :-
    var(Var),
    !,
    MessI = MessO,
    Args = Loc.
debug_call_location_exception_message( locate(cannot_locate(Spec,Ext)), _Loc, _MessI, Mess, Args ) :-
    Mess = 'Could not locate file specified by: ~w, and extensions: ~w',
    Args = [Spec,Ext].
debug_call_location_exception_message( Error, _Loc, _MessI, _Mess, _Args ) :-
    % fixme:
    throw( debug_call_caught(Error) ).

debug_mtx_dims( [], 0, 0 ) :-
    !.
debug_mtx_dims( Rows, NRows, NCols ) :-
    length( Rows, NRows ),
    Rows = [Hdr|_],
    ( is_list(Hdr) -> length(Hdr,NCols); functor(Hdr,_,NCols) ).

debug_message_prefixed( '', Standard, Standard ) :- !.
debug_message_prefixed( Pfx, Standard, Prefixed ) :-
    sub_atom( Standard, 0, 1, Aft, Fst ),
    downcase_atom( Fst, Low ),
    sub_atom( Standard, 1, Aft, 0, Right ),
    atomic_list_concat( [Pfx,' ',Low,Right], Prefixed ).
