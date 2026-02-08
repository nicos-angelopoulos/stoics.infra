:- module( debug_call, 
        [ 
            debug_call/2,
            debug_call/3,
            debug_call/4,
            debuc/1, debuc/2, debuc/3, debuc/4,
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

Debugging information focusing on avoiding running goals to produce output that is only relevant while debugging.

Includes pre-canned, often used calls that print informative messages for common debugging tasks.

See the main predicate's documenation, debug_call/4, for more details.
See file examples/exo.pl for a full pallette of examples.

---+++ Examples

==

?- debug(ex).
?- debug_call(ex, length, list1/[x,y,z]).
% Length for list, list1: 3

?- debug_call(ex, length, [list1,list2]/[[x,y,z],[a,b,c]], prefix('Some prefix')).
% Some prefix lengths for lists, list1: 3, list2: 3

?- debug_call(ex, dims, [m1,m2]/[[a(x),a(y),a(z)],[xy(a,b),xy(c,d),xy(e,f)]]).
%  Dimensions for matrices,  (m1) nR: 3, nC: 1. (m2) nR: 3, nC: 2.

?- debug_call(ex, enum, testo/[a,b,c]).
% Starting enumeration of list: testo
% 1.a
% 2.b
% 3.c
% Ended enumeration of list: testo
true.

?- debug_call(ex, enum, testo/[a,b,c,d,e], depth(3)).
% Starting enumeration of list: testo
% 1.a
% 2.b
% 3.c
% ... + 2 other elements
% Ended enumeration of list: testo

?- debug_call(ex, info, 'My message is ~w.'/long).
% My message is long.
true.    
   % message above is printed in informational colour

?- debuc(ex, wrote, loc(file,csv)).
% Could not locate wrote on file specified by: file, and extensions: csv
?- csv_write_file('file.csv', []).

?- debuc(ex, version, debug_call).
% Using debug_call_version, at version: 2:1:1 (published on: date(2025,12,6)).

?- debuc(ex, wrote, loc(file,csv)).
% Wrote on file: 'file.csv'

?- debuc(ex, task(stop), 'write on file').
At 15:44:1 on 2nd of Jul 2024 finished task: write on file.

?- debuc(ex, task(stop), 'talking ~w', [farg(point)]).
% At 13:58:50 on 6th of Dec 2025 stop task: talking point

?- assert((simple_mess(KVs,Mess,Args):- KVs =[a=A,b=B], atom_concat(A,B,Mess), Args=[])).
?- debuc(ex, simple_mess([a=1,b=2])).
% 12
true.

?- debuc(ex, stat, runtime, true).
% stat(runtime,[182,4]).
true.

?- debuc(ex, stat, runtime, [check_point(here),comment(false)]).
stat(here,runtime,[193,11]).
true.

?- debuc(ex, stat, testo, [sub(true),comment(false)]).
duh([testo-8.0K,testo/file_a-0,testo/sub_1-4.0K]).
==

---+++ Variable topics 

This library avoids the messy way in which package(debug) deals with variable debug topics. 
That is, their term expansion and subsequent pattern matching mishandles goals of the form
debugging/1 and debug/3 that have an unbound variable in the 1st argument.
debug_calls uses dynamic =..  .


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
@version 1.3 2020/3/7
@version 1.4 2020/9/18
@version 1.5 2022/12/29
@version 2.0 2025/10/7
@version 2.1 2025/10/27
@version 2.2 2025/12/8
@see debug_call/4 for information on what each version added.

*/

/** debug_call_version( -Version, -Date ).

Current version and release date for the library.

==
?- debug_call_version( -V, -D ).
V = 2:2:0,
D = date(2025,12,8).
==
*/
debug_call_version(2:2:2, date(2026,1,11)).

:- use_module(library(apply)).       % maplist/4,...
:- use_module(library(lists)).       % member/4,...
:- use_module(library(debug)).       % debug/1,...
:- use_module(library(filesex)).     % directory_member/3.
:- use_module(library(readutil)).    % read_line_to_codes/2.
:- use_module(library(process)).     % process_create/3.
:- use_module(library(readutil)).    % read_line_to_codes/2.
:- use_module(library(prolog_pack)). % pack_property/2.
:- use_module(library(lib)).

:- lib(source(debug_call), [homonyms(true),index(false)]).
:- lib(stoics_lib:locate/3 ).
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:message_report/3).
:- lib(stoics_lib:datime_readable/1).
:- lib(end(debug_call) ).

%% debuc(+Topic).
%% debuc(+Topic, +Goal).
%% debuc(+Topic, +Goal, +Args).
%% debuc(+Topic, +Goal, +Pfx, +Args).
% 
% Shorthands for debug_call/2,3,4 and debug/1.
% 
%@author nicos angelopoulos
%@version  0:1 2020/9/9
%
debuc( Topic ) :-
    debug( Topic ).
debuc( Topic, Goal ) :-
    debug_call( Topic, Goal ).
debuc( Topic, Goal, Arg ) :-
    debug_call( Topic, Goal, Arg ).
debuc( Topic, Goal, Arg, Opts ) :-
    debug_call( Topic, Goal, Arg, Opts ).

%% debug_call( +Topic, +Goal ).
%
%  Only call debug if we are debugging Topic.
%
%  If Goal with arity +2 is available call that instead of Goal with extra arguments Mess and Args
%  that will be passed to debug/3. If the goal (original or +2) fail, nothing is printed by
%  debug_call and the debug_call(T,G) itself succeeds.
% 
%==
% ?- goal( Goal, Mess, Args ).
%==
% 
% Examples
%==
% ?- debug(ex)
% ?- assert((simple_mess(KVs,Mess,Args):- KVs =[a=A,b=B], atom_concat(A,B,Mess), Args=[])).
% ?- debug_call(ex, simple_mess([a=1,b=2])).
% 12
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
% If already debugging TopicCond, then also start debugging TopicDep.
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
    en_list( Then, Thens ),
    maplist( debug_chain(Topic), Thens, _Priors ).

debug_chain( Topic, Then, Prior ) :-
    debugging_topic( Topic ),
    !,
    debugging_status( Then, Prior ),
    debug( Then ).
debug_chain( _Topic, _Then, true ). 
    % setting 3rd to true is a bit presumptious of its uses later on

/** debug_message(+Topic, +Mess, +Args).

A wrap around debug/3 that calls it by constructing the term on-the-fly. 
So that lib(debug) does not create a record by inspecting the term (via expansion).
Particularly useful in sending uninstantiated Topics.

@author nicos angelopoulos
@version  0.1 2016/11/1
@version  0.1 2026/1/25, avoid building the term with =.. use call/4 directly

*/
debug_message( Topic, Mess, Args ) :-
    call( debug, Topic, Mess, Args ).

/** debugging_topic( ?Topic ).

A wrap around debugging/1 that calls it by constructing the term on-the-fly. 
So that lib(debug) does not create a record by inspecting the term (via expansion).
Particularly useful in sending uninstantiated Topics.

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

%% debug_call( +Topic, +DebuGoal, +Arg ).
%% debug_call( +Topic, +DebuGoal, +Arg, +Opts ).
%
% Automates often used debug calls with emphasis on: (a) avoiding calling things that will not be reported and (b) easy tailoring of the messages.
% 
% The main novelty is the introduction of abbreviated Goals, that print bespoke messages for often used debugging information. 
% For example the following code ejects info on the legth of the list. Not only the code for calculating the length
% only happens if debugging for the topic ex, is on, but the message is also tailored to reporting lengths of lists.
%==
% ?- debug(ex).
% ?- debug_call(ex, length, math_vars/[x,y,z]).
% % Length for list, math_vars: 3
%==
% With v1.3 the debuc/n shorthand was introduced. So debuc/1,2,3,4 are shorthands for debug_call/1,2,3,4.
% 
% ==
% ?- Mtx = [h(a,b,c),r(1,2,3),r(4,5,6),r(7,8,9)],
%    debuc(ex, dims, mtx/Mtx).
% 
% Dimensions for matrix, mtx: nR: 4, nC: 3.
% ==
% 
% The predicate can work as a replacement to debug/3. That is, if Goal does not match any of the forms below, it will be interpreted as a message.
% ==
% ?- debuc(ex, 'A simple message in a ~a.', [bottle] ).
% A simple message in a bottle.
% ==
% 
% The predicate can be used to call arbitrary Goal and then print a message after it has successfull completed (see below).<br>
% When Goal is a known abbreviation from those shown below, the Arg usually qualifies the output generated.
%
%  As of v2 the last two arguments of the /4 version of the predicate were switched from _Pfx_ and Arg
%  to Arg and Opts. Opts pass arbitary things to Goal, each abbreviation Goal can demand different options. 
%  All debuc Goals can take =|prefix(Pfx)|= which corresponds to Pfx in the old /4 version, and =|pred(Fnc,Ar)|= or =|pred(Pid)|=.
%==
% ?- debuc(ex, enum, list_x/[x1,x1,x3], [pred(integral,2),prefix('At')] ).
% % At predicate: integral/2 starting enumeration of list: list_x
% % 1.x1
% % 2.x1
% % 3.x3
% % At predicate: integral/2 ended enumeration of list: list_x
%==
% The predicate is relaxed about Opts. It can be a single term, which will be cast into a list.
%==
% ?- debuc(ex, pwd, my_run, pred(bio_db,3) ).
%
% Predicate: bio_db/3 pwd at, my_run, is: '/home/nicos/pl/packs/private/debug_call/'
%==
%
% Goal in:
%  * call(Goal)
%    Call Goal before printing debugging message debug(Topic, Mess, MArgS).  Goal is called in deterministic context.
%    Goal is called with extra arguments +Arg, -Mess and -MArgS.
%  * call(Goal,Opts)
%    As above, but Opts are passed as an extra, last argument in the call.
%  * dims
%    Prints the dimensions of matrix, see mtx_dims/3.
%  * duh
%    Short for shell command 'du -h -s' which is disc usage, summarized and in human readable form.
%    Arg should be the directory to report the size of.
%    Can take options check_point() and comment()- as does =|stat|=.
%    The latter allows for reporting without '%' so terms can be read in by read/1 or consulted.
%    In addition, option sub() can be used the subdirectories of 
%  * end
%    Translates to finishing ~Arg or finishing ~Topic if =|Arg==true|=.
%  * enum
%    Print members of lists and arguments of terms, where each item is printed on single line and prefixed by an index number
%    Knows: depth(Depth) (restricts items to print).
%  * goal
%    Anything that does n't match any of the above is retrived as call(Goal)
%  * info
%    Print using informational machinery (usually different/green colour, to debug's blue)
%    term should Mess/Args in the debug/3 version
%  * input
%    Reports reading from a file. Arg should be file specification suitable for locate/3.
%    Either loc(File,Exts) or simply File in which case Exts = ''.
%    As of v2.0 the default is to print the basename, use path(abs) in Opts if the full path to the file is needed.
%  * length
%    Prints the lenghts of a bunch of lists. Args should be ListNames/Lists. 
%    uses non list ListNames if debuging the length of a single list, in which case
%    message in the singular is used.
%  * list
%    Writes contents of list with header and footer. Arg should be of the form Hdr/Ftr/List, 
%    else Hdr/List unfolds to Hdr/Hdr/List and List is translated to unknown/unknown/List. 
%    Knows: depth(Depth) (restricts items to list).
%  * ns_sel
%    First argument of Arg is the item selected from second arg which is expected to be a list.
%    The selected argument can be named on the massage via sel_name(Lnm) in Opts.
%  * odir
%    Output directory (Arg should exist and be a directory)
%  * option
%    Option selected from options for predicate. Possible options: pred(Fnc,Ar) or pred(Pid), the caller predicate, all(OrigOpts), shows all options, 
%    internal(true), shows also '$' starting options.
%  * options
%    Options used on call to a predicate. Possible options: pred(Func,Ar), pred(Pid), the caller predicate, internal(true), shows also '$' starting options.
%  * pwd 
%    Message about current dir Location (=Arg), (if =|Arg==false|=, location is not shown)- see examples.
%  * read
%    Alias for input.
%  * session
%    Print info on all loaded code, packs are shown with versions both from packs and info from <Pred>_version/2,3 if available (as per stoics.org.uk packs).
%  * start 
%    Translates to starting ~Arg or starting ~Topic if =|Arg==true|=.
%  * stat
%    Arg should be a key that can be used as the 1st arg for statistics/1, the debuc Goal reports the statistic.
%    Can take options check_point() and comment(). 
%    The latter allows for reporting without '%' so terms can be read in by read/1 or consulted.
%  * task(Wch)
%    Time of start/stop (Wch) of a task. Other values for Wch are allowed but printed as they come. 
%    Arg can be a term (as of Version 1.5).
%    Arg can include formatting tidles, and associated arguments should be passed via farg() option.
%  * term
%    Report the input term. The term can be named via option term_name(Tnm).
%  * var
%    Reports variable name (arg(1)) and its current instantiation (arg(2))
%  * version
%    Reports version of software currently using:
%    * predicate name of <Arg>_version/2,3 or <Arg>/2 (1st arg Version, 2nd arg release date),
%    * Arg should be a term of arity 3 (Sw/Vers/Date) or arity 2 (Sw/Vrs/Date).
%  * wrote 
%    Reports the writting of output on a file. Arg should be file specification suitable for locate/3.
%    Either loc(File,Exts) or simply File in which case Exts = ''.
%    As of v2.0 the default is to print the basename, use path(abs) in Opts if the full path to the file is needed.
%
% As of v2.1 almosst all debuc Goals work with options prefix(Pfx) and pred(Ar,Fn) (also synonymed to pred(Pid)).
% 
% v2.2 introduced ability to pass formatting patterns and arguments via farg() option. 
%
% See file examples/exo.pl for a test suit including at least one example from each debuc Goal.
% 
% Opts supported by all debugoals
%  * prefix(Pfx)
%    prefixes the message
%  * pred(F,A)
%    (also pred(F/A)) adds predicate caller identfication to message
%  * farg(Farg)
%    pass formating argument when Arg contains '~' formatting terms that expecdt an argument [task(start),task(stop)]
% 
% Opts for specific debugoals
%  * check_point(Point)
%    adds check point info to message [stat]
%  * comment(Comm)
%    when =|Comm==false|= print message through format without '%' (portray_message) machineary [stat- print readable terms]
%  * depth(Depth)
%    limits output times [enum,list]
% 
%==
% ?- debug(ex).
% 
% ?- debuc( ex, (length([a,b,c],L),write(len(L)),nl) ).
% len(3)
% L = 3.
%
% ?- debug_call(ex, length, list1/[x,y,z]).
% % Length for list, list1: 3
%  
% ?- debug_call(ex, length, [list1,list2]/[[x,y,z],[a,b,c]] prefix('some prefix')).
% % some prefix lengths for lists, list1: 3, list2: 3
% 
% ?- debuc(ex, wrote, loc(file,csv)).
% % Could not locate wrote on file specified by: file, and extensions: csv
%
% ?- csv_write_file( 'file.csv', []).
% ?- debuc(ex, wrote, loc(file,csv)).
% % Wrote on file: 'file.csv'
%
% ?- debuc(ex, wrote, loc(file,csv), path(abs)).
% % Wrote on file: '/home/nicos/pl/lib/src/trace/file.csv'
%
% ?- debuc(ex, task(stop), 'write on file').
% % At 15:44:1 on 2nd of Jul 2014 finished task: write on file.
%    
% ?- debuc( ex, pwd, here ).
% % Pwd at, here, is: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-publish/bio_db_repo-20.09.14/data/hs/maps/hgnc/'
% true.
% 
% ?- debuc( ex, pwd, false ).
% % Pwd: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-publish/bio_db_repo-20.09.14/data/hs/maps/hgnc/'
% true.
% 
% ?-  Etcs = [suv-17.09.26.txg,suv-17.09.21.txg], Etc = suv-17.09.26.txg,
%     debuc(suv, ns_sel, c(Etc,Etcs, sel_name('suv file') ).
% Continuing with: suv-17.09.26.txg as the: suv file. From list: [suv-17.09.26.txg,suv-17.09.21.txg]
%
% ?- assert( (list_avg_mess(List,Mess,Args) :- length(List,Len), sum_list(List,Sum), Avg is Sum / Len, Mess = 'Avg: ~w', Args = Avg) ).
% ?- debuc( ex, call(list_avg_mess), [1,2,3] ).
% Avg: 2
%
% ?- debuc( ex, call(list_avg_mess), [1,2,3], prefix('By call') ).
% By call avg: 2
%
% ?- debuc( ex, call(list_avg_mess), [1,2,3], [pred(p1,2),prefix('By call')] ).
% By call predicate: p1/2 avg: 2
%
% ?- debuc(ex, stat, runtime, true).
% stat(runtime,[182,4]).
% true.
% 
% ?- debuc(ex, stat, runtime, [check_point(here),comment(false)]).   % note this does not have a precedding percentage char
% stat(here,runtime,[193,11]).   
% 
%==
% 
% At some point around SWI-Prolog 8, behaviour of debug/3 changed in being more strict about messages with no arguments.
% As of version 1.2 debug_call/3 can act as a replacement of debug/3 but with the old behaviour.
%
%==
% ?- debug(ex, 'Messaging...', true).
% Messaging...
% [[ EXCEPTION while printing message 'Messaging...'
%       with arguments user:true:
%       raised: format('too many arguments')
%    ]]
% 
% true.
% 
% ?- debuc(ex, 'Messaging...', true).
% % Messaging...
% true.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/03/27
% @version  0.2 2014/04/24  added wrote
% @version  0.3 2014/07/2   added task
% @version  0.4 2014/09/22  renamed from debug_call/3
% @version  0.5 2014/??/??  added ns_sel
% @version  1.1 2018/03/20  prefer +2 arity in debug_call/2
% @version  1.2 2020/03/07  now can be used as a replacement for debug/3 (but with old 3rd arg behaviour, allowing eg 'true').
% @version  1.3 2020/09/14  added debuc Goal info and enum, debuc/2,3,4
% @version  2.0 2025/10/07  changed last two arguments, new option goal recogniser, pred/1, internal/1 & all/1
% @version  2.1 2025/10/27  pid(F,A) & prefix() universal; call() fixed; doc; enum terms fix; ns_sel simplify
% @version  2.2 2025/12/08  farg() option; depth() option in list and enum; debuc Goals: version, session.
% @version  2.3             debuGoal stat; opts comment(t/f), check_point(), duh (opts as stat + sub(D))
% @see file examples/exo.pl
% @see debuc/3 shorthand for debug_call/3
%
debug_call( Topic, Goal, Arg ) :-
    debug_call( Topic, Goal, Arg, [] ).

debug_call( Topic, Goal, Arg, OptsPrv ) :-
    debugging_topic( Topic ),
    !,
    en_list( OptsPrv, Opts ),
    debugging_call( Topic, Goal, Arg, Opts ).
debug_call( _Topic, _Goal, _Arg, _Opts ).

debugging_call( Topic, call(Goal), Arg, Opts) :-
    !,
    call( Goal, Arg, Gess, Grgs ),
    !,
    debug_call_message_opts( Gess, Grgs, Mess, Args, Opts ),
    debug_message( Topic, Mess, Args ).
debugging_call( Topic, call_opts(Goal), Arg, Opts ) :-
    !,
    call( Goal, Arg, Gess, Grgs, Opts ),
    debug_call_message_opts( Gess, Grgs, Mess, Args, Opts ),
    debug_message( Topic, Mess, Args ).
debugging_call( Topic, Goal, Arg, Opts ) :- 
    debug_call_topic( Goal, Arg, Opts, Topic ),
    !.
debugging_call( Topic, Goal, Mess, Args ) :-
    compound( Goal ),
    call( Goal ),
    !,
    debug_message( Topic, Mess, Args ).
% 20.03.07: this makes debug_call/3 a replacement for debug/3...
debugging_call( Topic, Mess, ArgsPrv, _DbgCallArgs ) :-
    % as of SWI-Prolog 8.?.? there is an error thrown when true is used instead of [] as 3rd arg of debug/3
    atomic( Mess ),
    !,
    ( ArgsPrv == true -> Args = []; en_list(ArgsPrv,Args) ),
    debug( Topic, Mess, Args ).
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

debug_call_topic( info, Arg, Bogs, _Topic ) :-
     ( (\+ var(Arg),Arg = Mess/Args) ->
          true
          ;
          % fixme: not sure what to do here ?
          Mess = Arg,
          Args = []
     ),
     debug_call_message_opts( Mess, Args, Prefixed, Prgs, Bogs ),
	phrase('$messages':translate_message(debug(Prefixed,Prgs)), Lines),
	print_message_lines(current_output, kind(informational), Lines).
debug_call_topic( dims, NamesPrv/MtxsPrv, Bogs, Topic ) :-
    ( is_list(NamesPrv) -> Names=NamesPrv, MtxsPrv=Mtxs, With = 'Dimensions for matrices, '
                           ; [NamesPrv] = Names, [MtxsPrv]=Mtxs, With = 'Dimensions for matrix, ' 
    ),
    maplist( debug_mtx_dims, Mtxs, NRows, NCols ),
    findall( PartM, (member(_,Names),PartM='~w: nR: ~d, nC: ~d.'), MParts ),
    atomic_list_concat( MParts, '', Right ),
    findall( [Name,NRow,NCol], (nth1(N,Names,Name),nth1(N,NRows,NRow),nth1(N,NCols,NCol)), NNest ),
    flatten( NNest, Vargs ),
    atom_concat( With, Right, Vess ),
    debug_call_message_opts( Vess, Vargs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( enum, InArg, Bogs, Topic ) :-
    ground( InArg ),
    ( InArg = Left/Term -> true; Left = unnamed, Term = InArg ),
    ( is_list(Term) ->
        length( Term, Len ),
        number_codes( Len, LenCs ),
        length( LenCs, SpcLen ),
        debug_call_topic_list_delim( Left, Topic, 'Starting enumeration of list: ~w', Bogs ),
        ( memberchk(depth(Depth), Bogs) -> true; Depth = inf ),
        debug_call_topic_enum( Term, 1, Depth, SpcLen, Topic ),
        debug_call_topic_list_delim( Left, Topic, 'Ended enumeration of list: ~w', Bogs )
        ;
        Term =.. [Func|Args],
        length( Args, Len ),
        number_codes( Len, LenCs ),
        length( LenCs, SpcLen ),
        atomic_list_concat( ['Starting enumeration of term: ~w (func: ',Func,')'], StrMess ),
        debug_call_topic_list_delim( Left, Topic, StrMess, Bogs ),
        ( memberchk(depth(Depth), Bogs) -> true; Depth = inf ),
        debug_call_topic_enum( Args, 1, Depth, SpcLen, Topic ),
        atomic_list_concat( ['Ended enumeration of term: ~w (func: ',Func,')'], EndMess ),
        debug_call_topic_list_delim( Left, Topic, EndMess, Bogs )
    ).
debug_call_topic( length, NamesPrv/ListsPrv, Bogs, Topic ) :-
    ( is_list(NamesPrv) -> Names=NamesPrv, ListsPrv=Lists, With = 'Lengths for lists, '
                           ; [NamesPrv] = Names, [ListsPrv]=Lists, With = 'Length for list, ' 
    ),
    maplist( length, Lists, Lengths ),
    findall( ['~w: ~w',', '], member(_,Lengths), WsNest ),
    flatten( WsNest, WsL ),
    once( append(WsLComma,[_],WsL) ),
    append( WsLComma, ['.'], WsLDot ),
    atomic_list_concat( WsLDot, '', Right ),
    findall( [Name,Length], (nth1(N,Names,Name),nth1(N,Lengths,Length)), NLNest ),
    flatten( NLNest, NLs ),
    atom_concat( With, Right, Vess ),
    debug_call_message_opts( Vess, NLs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ). % do the messaging
debug_call_topic( list, InArg, Bogs, Topic ) :-
    ground( InArg ),
    ( InArg = Left/List -> 
        ( Left = Hdr/Ftr -> true ; Hdr = Left, Ftr = Left )
        ;
        List = InArg, Hdr = unamed, Ftr = unamed
    ),
    debug_call_topic_list_delim( Hdr, Topic, 'Starting listing of list: ~w', Bogs),
    ( memberchk(depth(Depth),Bogs) -> 
          length( Clean, Depth ),
          ( append(Clean,[H|T],List) ->
                    maplist( debug_message(Topic,'~w'), Clean ),
                    length( [H|T], Xen ),
                    ( Xen =:= 1 -> 
                         Mess = '... + ~d other element'
                         ;
                         Mess = '... + ~d other elements'
                    ),
                    debug_message( Topic, Mess, Xen )
                    ;
                    maplist( debug_message(Topic,'~w'), List )
          )
          ;
          maplist( debug_message(Topic,'~w'), List )
    ),
    debug_call_topic_list_delim( Ftr, Topic, 'Ended listing of list: ~w', Bogs ).
debug_call_topic( odir, Odir, Bogs, Topic ) :-
    ( exists_directory(Odir) ->
        Mess = 'Output in directory: ~w'
        ;
        Mess = 'Output (claimed) in (non-existing) directory: ~w'
    ),
    debug_call_message_opts( Mess, [Odir], Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( option, Opt, Bogs, Topic ) :-
    Ness = 'Option selected: ~w',
    ( (memberchk(all(OrgOpts),Bogs),is_list(OrgOpts)) ->
               ( memberchk(internal(true),Bogs) ->
                    RdcOpts = OrgOpts
                    ;
                    findall( R, (member(R,OrgOpts),functor(R,F,_),\+(atom_concat('$',_,F))), RdcOpts )
               ),
               atom_concat( Ness, ' from options: ~w', Mess ),
               Mrgs = [Opt,RdcOpts]
               ;
               atom_concat( Ness, '.', Mess ),
               [Opt] = Mrgs
    ),
    debug_call_message_opts( Mess, Mrgs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( options, RepOpts, Bogs, Topic ) :-
    Ness = 'Options: ~w',
    ( memberchk(internal(true),Bogs) -> 
               RepOpts = RdcOpts
               ;
               findall( R, (member(R,RepOpts),functor(R,F,_),\+(atom_concat('$',_,F))), RdcOpts )
    ),
    debug_call_message_opts( Ness, [RdcOpts], Message, Args, Bogs ),
    debug( Topic,  Message, Args ).
debug_call_topic( duh, Dir, Bogs, Topic ) :-
    debug_call_duh_dir( Dir, PaPair ),
    ( memberchk(sub(true), Bogs ) ->
          findall( Os, directory_member(Dir, Os, []), Oses ),
          maplist( debug_call_duh_dir, Oses, SubPairs ),
          Pairs = [PaPair|SubPairs] 
          ; 
          Pairs = [PaPair]
    ),
    ( memberchk(check_point(Point),Bogs) ->
          Rec = duh(Point,Pairs)
          ;
          Rec = duh(Pairs)
    ),
    debuc_call_topic_term( Rec, Topic, Bogs ).
debug_call_topic( stat, Key, Bogs, Topic ) :-
    statistics( Key, Stat ),
    ( memberchk(check_point(Point),Bogs) ->
          Rec = stat(Point,Key,Stat)
          ;
          Rec = stat(Key,Stat)
    ),
    debuc_call_topic_term( Rec, Topic, Bogs ).
debug_call_topic( term, Derm, Bogs, Topic ) :-
    ( memberchk(term_name(Tnm),Bogs) -> 
          Mess = 'Reporting term (~w): ~w',
          Mrgs = [Tnm,Derm]
          ; 
          Mess = 'Reporting term: ~w',
          Mrgs = [Derm]
    ),
    debug_call_message_opts( Mess, Mrgs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( var, DbgTerm, Bogs, Topic ) :-
    arg( 1, DbgTerm, Var ),
    arg( 2, DbgTerm, Val ),
    Mess = 'Variable: ~a, value: ~w',
    debug_call_message_opts( Mess, [Var,Val], Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( session, _Derm, Bogs, Topic ) :-
     ( memberchk(in_module(Mod), Bogs) -> true; Mod = user),
     current_prolog_flag( version_data, Swi ),  % Swi = swi(9, 3, 34, []).
     Swi  = swi(Mj,Mn,Fx,Inc),
     ( Inc == [] -> 
          atomic_list_concat( [Mj,Mn,Fx], ':', Vers )
          ;
          atomic_list_concat( [Mj,Mn,Fx,Inc], ':',Vers )
     ),
     debug_message( Topic, 'Session Info', [] ),
     ( current_prolog_flag(version_git, Git) -> % Git = '9.3.34-41-g8cf975236'.
          atomic_list_concat( ['Interpreter is SWI-Prolog ',Vers,', [Git: ',Git,'].'], Mess )
          ;
          atomic_list_concat( ['Interpreter is SWI-Prolog ',Vers,'.'], Mess )
     ),
     debug_call_message_opts( Mess, [], Message, Args, Bogs ),
     debug_message( Topic, Message, Args ),
     % find where alias pack points to
     once( (file_search_path(pack,PackPath),atomic(PackPath)) ),
     debug_call_topic_session_predicate_file_prefixed( PackPath, Mod, pack, Lacks ),
     findall( APack-ItsVers, (member(APack,Lacks),pack_property(APack,version(ItsVers))), PVs ),
     findall( Stoic-VersInfo, (  Mod:predicate_property(P,file(_)),
                                 functor(P,Fun,Ari),
                                 atom_concat(Stoic,'_version',Fun),
                                 ( Mod:predicate_property(P,imported_from(Stoic)) ->
                                    true
                                    ;
                                    Mod:predicate_property(P,exported)
                                 ),
                                 ( Ari =:= 3 -> 
                                        G =.. [Fun,Ser,Sdt,_],
                                        call(Mod:G)
                                        ;  % defaulty 2
                                        Ari =:= 2,
                                        G =.. [Fun,Ser,Sdt],
                                        call(Mod:G)
                                  ),
                                  VersInfo = (Ser @< Sdt)
                              ),
                                 SVs ),
     ( SVs == [] ->
          true
          ;
          ( SVs == [_] ->
               debug_message( Topic, 'Pack with predicated version info.', [] )
               ;
               debug_message( Topic, 'Packs with predicated version info.', [] )
          ),
          debug_call_topic_versions_predicated( SVs, PVs, Topic, RemPVs )
     ),
     ( RemPVs = [] -> 
          true
          ;
          ( RemPVs = [_] ->
               debug_message( Topic, 'Pack with version from pack file only.', [] )
               ;
               debug_message( Topic, 'Packs with version from pack file only.', [] )
          ),
          findall( _, (member(P-V,RemPVs),debug_message(Topic,'~w-~w',[P,V])), _ )
     ),
     once( (file_search_path(swi,SwiPath),atomic(SwiPath)) ),
     debug_call_topic_session_predicate_file_prefixed( SwiPath, Mod, boot, Boots ),
     debug_message( Topic, 'System boot files loaded.', [] ),
     findall( _, (member(Boot,Boots),debug_message(Topic,'~w',[Boot])), _ ),
     directory_file_path( SwiPath, library, LibPath ),
     debug_call_topic_session_predicate_file_prefixed( LibPath, Mod, rel, Libs ),
     ( Libs = [] ->
          debug_message( Topic, 'No system libraries found loaded.', [] )
          ;
          ( Libs =[_] ->
               debug_message( Topic, 'System library loaded.', [] )
               ;
               debug_message( Topic, 'System libraries loaded.', [] )
          ),
          findall( _, (member(Lib,Libs),debug_message(Topic,'~w',[Lib])), _ )
     ),
     findall( AppF, (   Mod:predicate_property(_,file(AppF)),
                        ( atom_concat(PackPath,PackPsfx,AppF) ->
                                            % fixme: check PackTop below, against loaded ?
                                   catch( atomic_list_concat([_Empty,_PackTop,TopSub|_],'/',PackPsfx),_,fail),
                                   \+ memberchk(TopSub,[prolog,src])
                                   ;
                                   true
                        ),
                        \+atom_concat(SwiPath,_,AppF)
                    ),
                         AppFsL ),
     sort( AppFsL, AppFs ),
     ( AppFs = [] ->
          debug_message( Topic, 'There where no application files loaded.', [] )
          ;
          ( AppFs = [_] ->
               debug_message( Topic, 'There is one application file loaded.', [] )
               ;
               debug_message( Topic, 'Application files loaded.', [] )
          ),
          findall( AnAF, (member(AnAF,AppFs),debug_message(Topic,'~w',[AnAF])), _ )
     ),
     debug_message( Topic, 'Session Info End', [] ).
debug_call_topic( version, Derm, Bogs, Topic ) :-
     ( atomic(Derm) -> 
          atom_concat( Derm, '_version', Verm ),
          ( current_predicate(Verm/2) ->
               Goal =.. [Verm,V,D],
               once(Goal)
               ;
               ( current_predicate(Verm/3) ->
                    Goal =.. [Verm,V,D,_],
                    once(Goal)
                    ;
                    ( pack_property(Derm,version(V)) ->
                         D = no_date
                         ;
                         ( current_predicate(Derm/2) -> 
                              Goal =.. [Derm,V,D],
                              once(Goal)
                              ;
                              V = no_vers, D = no_date
                         )
                    )
               )
          ),
          Mrgs = [Derm,V,D]
          ;
          ( functor(Derm,_,2) ->
               arg( 1, Derm, Sw ),
               arg( 2, Derm, V ),
               Mrgs = [Sw,V,no_date]
               ;
               ( functor(Derm,_,3) ->
                    arg( 1, Derm, Sw ),
                    arg( 2, Derm, V ),
                    arg( 3, Derm, D ),
                    Mrgs = [Sw,V,D]
                    ;
                    Mrgs = [Derm,no_vers,no_date]
               )
          )
     ),
     ( D == no_date ->
          (   V == no_vers ->
               Mess = 'Using ~w (no version or publication date available).',
               Mrgs = [A|_],
               Crgs = [A]
               ;
               Mess = 'Using ~w, at version: ~w.',
               Mrgs = [A,B|_],
               Crgs = [A,B]
          )
          ;
          Mess = 'Using ~w, at version: ~w (published on: ~w).',
          Mrgs = Crgs
     ),
     debug_call_message_opts( Mess, Crgs, Message, Args, Bogs ),
     debug_message( Topic, Message, Args ).
debug_call_topic( wrote, ForLoc, Bogs, Topic ) :-
    ( ForLoc = loc(Spec,Ext) -> true; Spec=ForLoc, Ext = '' ),
    catch( locate(Spec,Ext,Loc), Excp, true ),
    MessW = 'Wrote on file: ~p',
    debug_call_location_exception_message( Excp, write, Loc, MessW, Mess, Bogs, Mrgs ),
    debug_call_message_opts( Mess, Mrgs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( read, ForLoc, Bogs, Topic ) :-
     debug_call_topic( input, ForLoc, Bogs, Topic ).
debug_call_topic( input, ForLoc, Bogs, Topic ) :-
    ( ForLoc = loc(Spec,Ext) -> true; Spec=ForLoc, Ext = '' ),
    catch( locate(Spec,Ext,Loc), Excp, true ),
    MessW = 'Input from file: ~p',
    debug_call_location_exception_message( Excp, input, Loc, MessW, Mess, Bogs, Mrgs ),
    debug_call_message_opts( Mess, Mrgs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( task(Whc), TaskPrv, Bogs, Topic ) :-
    datime_readable( Readable ),
    debug_call_topic_time_which_readable( Whc, Whcable ),
    ( compound(TaskPrv) -> term_to_atom( TaskPrv, Task ); TaskPrv = Task ),
    atomic_list_concat( [Readable,' ',Whcable,' task: ',Task,'.'], '', Mess ),
    debug_call_message_opts( Mess, [], Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( start, Arg, Bogs, Topic ) :-
    Mess = 'Starting: ~w',
    ( Arg == true -> Rep = Topic; Rep = Arg ),
    debug_call_message_opts( Mess, [Rep], Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( end, Arg, Bogs, Topic ) :-
    Mess = 'Finished: ~w',
    ( Arg == true -> Rep = Topic; Rep = Arg ),
    debug_call_message_opts( Mess, [Rep], Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( pwd, Stage, Bogs, Topic ) :-
    working_directory( Pwd, Pwd ),
    ( Stage == false -> 
        Mess = 'Pwd: ~p', Mrgs = [Pwd]
        ;
        Mess = 'Pwd at, ~w, is: ~p', Mrgs = [Stage,Pwd]
    ),
    debug_call_message_opts( Mess, Mrgs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).
debug_call_topic( ns_sel, Term, Bogs, Topic ) :-
    arg( 1, Term, Fst ),
    arg( 2, Term, Sec ),
    ( memberchk(sel_name(Trd),Bogs) ->
          Mess = 'Continuing with: ~w as the: ~w. From list: ~w',
          MArgs= [Fst,Trd,Sec]
          ;
          Mess = 'Continuing with: ~w from list: ~w',
          MArgs= [Fst,Sec]
    ),
    debug_call_message_opts( Mess, MArgs, Message, Args, Bogs ),
    debug_message( Topic, Message, Args ).

debug_call_topic_session_predicate_file_prefixed( Path, Mod, Iface, Lacks ) :-
     findall( APack, ( Mod:predicate_property(_Pead, file(File)), 
                             atom_concat(Path, Psfx, File), 
                             ( Iface == pack -> 
                                   atomic_list_concat(['',APack|_], '/', Psfx)
                                   ;
                                   ( atom_concat('/',APack,Psfx) -> 
                                        ( Iface == boot ->
                                             \+ atom_concat( library, _, APack )
                                             ;
                                             true
                                        )
                                        ;
                                        Psfx = APack
                                   )
                             )
                           ),
                              AllPacks ),
     sort( AllPacks, Lacks ).

debug_call_topic_versions_predicated( [], PVs, _Topic, RemPVs ) :-
     PVs = RemPVs.
debug_call_topic_versions_predicated( [Pack-SVers|T], PVs, Topic, RemPVs ) :-
     ( select(Pack-InfoVer,PVs,NxtPVs) ->
          debug_message( Topic, '~w-~w (Pack file version: ~w)', [Pack,SVers,InfoVer] )
          ;
          debug_message( Topic, '~w-~w', [Pack,SVers] ),
          PVs = NxtPVs
     ),
     debug_call_topic_versions_predicated( T, NxtPVs, Topic, RemPVs ).

debuc_call_topic_term( Rec, Topic, Bogs ) :-
    ( memberchk(comment(false),Bogs) ->
          format( '~q.\n', [Rec] )
          ;
          debug_call_message_opts( '~w.', [Rec], Message, Args, Bogs ),
          debug_message( Topic, Message, Args )
     ).

debug_call_topic_enum( [], _I, _Depth, _Len, _Topic ).
debug_call_topic_enum( [H|T], I, Depth, Len, Topic ) :-
    ( I > Depth -> 
          Rem = [],
          length( [H|T], HTen ),
          ( HTen =:= 1 -> 
               Mess = '... + ~d other element'
               ;
               Mess = '... + ~d other elements'
          ),
          debug_message( Topic, Mess, HTen )
          ;
          T = Rem,
          number_codes( I, ICs ),
          length( ICs, ICsLen ),
          PadLen is Len - ICsLen,
          findall( ' ', between(1,PadLen,_), Spcs ),
          atomic_list_concat( Spcs, '', Pad ),
          atomic_list_concat( [Pad,'~d.~w'], '', Mess ),
          debug_message( Topic, Mess, [I,H] )
    ),
    J is I + 1,
    debug_call_topic_enum( Rem, J, Depth, Len, Topic ).

debug_call_topic_list_delim( ListName, Topic, Std, Bogs ) :-
     debug_call_message_opts( Std, [ListName], Mess, Args, Bogs ), 
     debug_message( Topic, Mess, Args ).

debug_call_topic_time_which_readable( Wch, Wchable ) :-
    debug_call_topic_time_which_readable_known( Wch, Wchable ),
    !.
debug_call_topic_time_which_readable( Wch, Wch ).

debug_call_topic_time_which_readable_known( start, starting ).
debug_call_topic_time_which_readable_known( finish, finished ).

debug_call_location_exception_message( Var, _Dir, Loc, MessI, MessO, Opts, Args ) :-
    var(Var),
    !,
    MessI = MessO,
    ( memberchk(path(abs),Opts) ->
               Args = [Loc]
               ;
               file_base_name( Loc, Arg ),
               Args = [Arg]
    ).
debug_call_location_exception_message( locate(cannot_locate(Spec,Ext)), Dir, _Loc, _MessI, Mess, _Opts, Args ) :-
    atomic_list_concat( ['Could not locate',Dir,'file specified by: ~w, and extensions: ~w'], ' ', Mess ),
    Args = [Spec,Ext].
debug_call_location_exception_message( Error, _Dir, _Loc, _MessI, _Mess, _Opts, _Args ) :-
    % fixme:
    throw( debug_call_caught(Error) ).

debug_mtx_dims( [], 0, 0 ) :-
    !.
debug_mtx_dims( Rows, NRows, NCols ) :-
    length( Rows, NRows ),
    Rows = [Hdr|_],
    ( is_list(Hdr) -> length(Hdr,NCols); functor(Hdr,_,NCols) ).

debug_message_prefixed( [], Standard, Standard ) :- !.
debug_message_prefixed( '', Standard, Standard ) :- !.
debug_message_prefixed( prefix(Pfx), Standard, Prefixed ) :-
     !,
     debug_message_prefixed( [prefix(Pfx)], Standard, Prefixed ).
debug_message_prefixed( [H|T], Standard, Prefixed ) :-
    memberchk( prefix(Pfx), [H|T] ),
    !,
    debug_message_prefixed_atom( Pfx, Standard, Prefixed ).
debug_message_prefixed( _, Standard, Standard ).

debug_call_message_opts( Std, Srgs, Mess, Args, Opts ) :-
     debug_call_pred_in_opts_mess( Std, Srgs, Pess, Args, Opts ),
     debug_message_prefixed( Opts, Pess, Mess ).

debug_call_pred_in_opts_mess( Std, Opt, Prefixed, Prgs, Bogs ) :-
     en_list( Opt, Opts ),
     ( debug_call_pred_in_opts(Pid, Bogs)  ->
          Pfx = 'Predicate: ~w',
          debug_message_prefixed_atom( Pfx, Std, Prefixed ),
          PrgsPrv = [Pid|Opts]
          ;
          Prefixed = Std,
          PrgsPrv = Opts
     ),
     debug_call_pred_in_opts_mess_format_args( PrgsPrv, Prgs, Bogs ).

debug_call_pred_in_opts( Pid, Opts ) :-
    memberchk( pred(Fun,Ar), Opts ),
    !,
    Fun/Ar = Pid.
debug_call_pred_in_opts( Pid, Opts ) :-
    memberchk( pred(Pid), Opts ).

debug_call_pred_in_opts_mess_format_args( PrgsPrv, Prgs, Bogs ) :-
     findall( Farg, member(farg(Farg),Bogs), Fargs ),
     append( PrgsPrv, Fargs, Prgs ).

debug_message_prefixed_atom( Pfx, Standard, Prefixed ) :-
    sub_atom( Standard, 0, 1, Aft, Fst ),
    downcase_atom( Fst, Low ),
    sub_atom( Standard, 1, Aft, 0, Right ),
    atomic_list_concat( [Pfx,' ',Low,Right], Prefixed ).

debug_call_duh_dir( Dir, Pair ) :-
    setup_call_cleanup(
            process_create(path(du), ['-h','-s',Dir],
                           [ stdout(pipe(Out))
                           ]),
            read_line_to_codes( Out, LineCs ),
            close(Out)),
    atom_codes( Line, LineCs ),
    atomic_list_concat( [Size,Item], '\t', Line ),
    Pair = Item-Size.
