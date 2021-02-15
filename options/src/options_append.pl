
:- lib(debugging_status/2).

options_append_known_process_option( debug ).
options_append_known_process_option( version ).

/** options_append( +PredName, +OptS, -All ).
    options_append( +PredName, +OptS, -All, +OAopts ).

Look for PredName_defaults/1 (Defs) and if that exists append its argument to the end of OptS to get All.<br>
OptS is casted to a list before the append, so single terms are allowed as options.<br>
In addition, if file user_profile(stoics/options/PredName[.pl]) exists, its terms are appended between OptS and
the argument of PredName_defaults/1.<br>
Appending of the profile terms, can be by passed with options append_profile(false).<br>
Listens to debug(options_append).
 
The predicate can process debug(Dbg) a commonly used option (in All). When Dbg is set to true =|debug( PredName )|= is called.  <br>
The default value of Dbg is provided by PredName_defaults/1.

OptS and All are options of PredName, whereas OAopts are options of options_append/4 and control
how OptS are transformed into All.


OAopts term or list of
  * args(Opts=en_list(OptS,Opts))
     the input OptS, but guaranteed to be a list

  * append_profile(AppProf=true)
     if false do not append the profile options

  * arity(Arity= -1)
     the arity of the caller, only used for reporting type errors for now

  * atoms(-Atoms)
     removes atoms from input arguments, and instantiates them into Atoms. OptS == true,
     is an exception: Atoms is instantiated to []

  * check_types(ChkTypes=true)
     if the defaults predicate returns a default of options_types(OTypes),
     this is removed and used to check the types of the supplied options.
     OTypes should be OptName-Type pairs list with Type being one of those
     recognised by type/2 in pack(pack_errors).

  * extra_arg(Arg)
     multiple allowed. All Arg(s) so passed are added to the Args passed to the defaults predicate,
     but not to the generated options. Allows for instance to pass arguments of the call itself
     to the defaults predicate without those arguments being added to the Options list

  * foreign(-Foreign)
     instantiates to all input options that do not have matching default
     term structure

  * module(DbgMod=false)
    any value apart from _false_ signals DbgMod(PredName) is used in the module.

  * debug(Dbg=none)
    if _true_ then if DbgMod is _false_ then debug PredName and call prolog_debug:debug_topic(PredName).
    Else, debug(DbgMod(PredName) is enabled.
    if _false_ turn debugging off for this call.
    if _all_, then if DbgMod is _false_ debug(_) is enabled else debug(DbgMod(_)) is enabled.
    Else Dbg can be a debug term (other than none,false,true) or list of debug terms.
 
  * funnel(Proccess)
     as process() below,
     but leaves processed options in All. (By default both debug and version are passed as funnel.)

  * pack(Pack)
     caller pack. For now it is only used to report type errors

  * process(Proccess)
     with Proccess in
     * debug
        will turn on debugging according to debug/0,1,2 options, see below
     * version
        partially instantiation version(Vers) in Defs with the term <Pred>(PredVersion) where version(PredVersion) is
        in OptS

  * remove_types(Rtypes=true)
     to pass options_types(OTypes) to the result Options use Rtypes == false

 When processing debugging options in All, the first matching term of the following is used:
    * debug
        short for debug(true)
   
    * debug(Dbgs)
        short for debug(Dbgs,_Prior)

    * debug(Dbgs,Prior)
        Prior is the prior status of debug(PredName). For each element of Dbgs call RHS:

    * true
        same as debug(PredName)

    * false
        nodebug(PredName) same as

    * none
        true same as

    * all
        debug(_) same as
    * Other
        debug(Other) other

==
?- assert( demo_defaults(ls(true)) ).
?- options_append( demo, ls(false), All ).
All = [ls(false), ls(true)].
 
?- options_append( demo, debug, All, process(debug) ).
All = [ls(true)].
 
?- options_append( demo, [debug(true),ls(false)], All, [process(debug),debug(true)] ).
Turning debugging on for predicate handle: demo
All = [ls(false), ls(true)].
 
% Note that the debug(options_append) has been removed in the following:
?- options_append( demo, debug, All, process(debug) ).
All = [ls(true)].

?- [pack(options/examples/ex_app)].
?- ex_app.
atoms([a,b,c])
opts([frg1(false),opt1(false),opt1(true)])
foreign([frg1(false)])
true.


?- lib(os_lib).
?- os_sel( os_files, ext(svg), Oses, version(V) ).
Oses = [],
V = [os_sel(0:1:3), os_file(0:0:4)|_9214].

==
  
The default OAopts list is [funnel(debug)].
  
@author nicos angelopoulos
@version  0.2 2014/9/20
@version  0.3 2020/5/9, changed file location to: user_profile(stoics/options/PredName[.pl])
@version  0.4 2020/9/27, termed debug option
@tbd  add option! for making sure that only recognised options (with additional,"silent" defaults) are accepted ??
@see ~/bin/cline/keep.pl for debug option example
@see pack(options/examples/ex_app.pl)
@tbd allow for strict lists inputs
*/

options_append( Pname, Args, Opts ) :-
    options_append( Pname, Args, Opts, [funnel(debug),funnel(version)] ).
options_append( Pname, ArgS, Opts, OAoptS ) :-
    options_en_list( OAoptS, OAoptsWA0 ),
    append( OAoptsWA0, [funnel(debug),funnel(version)], OAoptsWA ),
    atom_concat( Pname, '_defaults', Dname ),
    ( ArgS == true ->
        ArgsPrv = []
        ;
        options_en_list( ArgS, ArgsPrv )
    ),
    ( select(args(ArgsPrv),OAoptsWA,OAoptsWA1) -> true; OAoptsWA = OAoptsWA1),
    ( select(atoms(Atoms),OAoptsWA1,OAoptsNA) ->
        % fixme: ensure Atoms is unbound
        partition( atomic, ArgsPrv, Atoms, Args )
        ;
        OAoptsWA = OAoptsNA,
        ArgsPrv = Args
    ),
    ( select(check_types(ChkTypes),OAoptsNA,OAoptsNT) -> true; ChkTypes=true, OAoptsNT=OAoptsNA ),
    ( select(remove_types(RmvTypes),OAoptsNT,OAoptsNR) -> true; RmvTypes=true, OAoptsNR=OAoptsNT ),
    ( select(arity(PArity),OAoptsNR,OAoptsAri) -> true; PArity= -1, OAoptsAri=OAoptsNR ),
    ( select(pack(Pack),OAoptsAri,OAoptsPack) -> true; Pack=[], OAoptsPack=OAoptsAri ),
    ( select(module(DbgMod),OAoptsPack,OAoptsDmod) -> true ;  DbgMod=false, OAoptsPack=OAoptsDmod ),
    ( select(append_profile(AppProf),OAoptsPack,OAoptsApF) -> true; AppProf=true, OAoptsApF=OAoptsDmod),
    OAopts = OAoptsApF,
    options_append_profile_options( AppProf, Pname, Args, ProfArgs ),
    findall( Xarg, member(extra_arg(Xarg),OAopts), Xargs ),
    append( ProfArgs, Xargs, ExtArgs ),
    options_def_append( Dname, Pname, ProfArgs, ExtArgs, Defs, OptsUnpro ),
    options_append_select_own_debug( OAopts, ProcessOpts, Restore ),
    options_append_process( ProcessOpts, OptsUnpro, Args, Defs, Pname, DbgMod, OptsWT ),
    options_append_types( ChkTypes, Pname/PArity, Pack, OptsWT ),
    options_append_types_remove( RmvTypes, OptsWT, Opts ),
    options_append_restore_debug_status( Restore ).

options_append_types( false, _Pid, _Pack, _Opts ) :- !.
options_append_types( _Defaulty, Pid, Pack, Opts ) :-
    memberchk( options_types(OTS), Opts ),
    !,
    options_en_list( OTS, OTs ),
    ( Pack == [] -> TypeOpts = [pred(Pid),option(OName)]; TypeOpts = [pred(Pid),pack(Pack),option(OName)] ),
    findall( _, (  member(OName-Type,OTs),
                Opt=..[OName,OArg],
                options(Opt,Opts),
                type(Type,OArg,TypeOpts)
                    ),
                        _ ).
options_append_types( _Defaulty, _Pid, _Pack, _Opts ).

options_append_types_remove( false, Opts, Opts ) :- !.
options_append_types_remove( _Defaulty, OptsWT, Opts ) :-
    options_remainder( OptsWT, remove_types, 1, _, Opts ).

% options_append_profile_options( +AddB, +Pname, +Args, -ProfArgs ),
%
% If AddB is true and file $HOME/.pl/Pname.pl exists, append its terms to Args.
%
options_append_profile_options( true, Pname, Args, Semi ) :-
    % JW: 16.11.14
    AbsOpts = [access(read), file_errors(fail), file_type(prolog)],
    directory_file_path( 'stoics/options', Pname, Bname ),
    absolute_file_name(user_profile(Bname), File, AbsOpts ),
    !,
    read_file_to_terms( File, Terms, [] ),
    append( Args, Terms, Semi ),
    !.
options_append_profile_options( _, _Pname, Args, Args ).

/*
options_append_args( Opts, ArgsList, Arity ) :-
    memberchk( args(Args), Opts ),
    !,
    ArgsList = [Args],
    Arity is 2.
options_append_args( _Opts, Args, Arity ) :-
    Args = [],
    Arity is 1.
*/

options_append_process( [], Opts, _Args, _Defs, _Pname, _DbgMod, Opts ).
options_append_process( [extra_arg(_)|T], All, Args, Defs, Pname, DbgMod, Opts ) :-
    !,
    options_append_process( T, All, Args, Defs, Pname, DbgMod, Opts ).
options_append_process( [process(Opt)|T], All, Args, Defs, Pname, DbgMod, Opts ) :-
    !,
    options_remainder( T, process, 1, Opt, Rem ),
    options_append_process_option( Opt, All, Pname, DbgMod, Args, Defs, Nxt, _Enh, Rem ),
    options_append_process( Rem, Nxt, Args, Defs, Pname, DbgMod, Opts ).
options_append_process( [funnel(Opt)|T], All, Args, Defs, Pname, DbgMod, Opts ) :-
    !,
    options_remainder( T, process, 1, Opt, Rem ),
    options_append_process_option( Opt, All, Pname, DbgMod, Args, Defs, _Nxt, Enh, Rem ),
    options_append_process( Rem, Enh, Args, Defs, Pname, DbgMod, Opts ).
options_append_process( [foreign(Fgn)|T], All, Args, Defs, Pname, DbgMod, Opts ) :-
    !,
    options_remainder( T, process, 1, Fgn, Rem ),
    exclude( template_in_defaults(Defs), All, Fgn ),
    options_append_process( Rem, All, Args, Defs, Pname, DbgMod, Opts ).
options_append_process( [Opt|_T], _All, _Args, _Defs, Pname, DbgMod, _Opts ) :-
    throw( unknown_option_in_options_append(Opt,Pname,DbgMod) ). % fixme

template_in_defaults( Defs, Term ) :-
    functor( Term, Tname, Tarity ),
    functor( Template, Tname, Tarity ),
    memberchk( Template, Defs ).

options_append_process_option( Opt, All, Pname, DbgMod, Args, Defs, Nxt, Enh, Rem ) :-
    options_append_known_process_option( Opt ),
    !,
    options_append_option_process( Opt, All, Pname, DbgMod, Args, Defs, Nxt, Enh, Rem ).
options_append_process_option( Opt, _All, _Pname, _DbgMod, _Args, _Defs, _Nxt, _Enh, _Rem ) :-
    throw( options_append( unknown_process_option(Opt)) ).

% user + program can use debug/0,1,2 the first one
% is only used, the Dbg terms argument can be a list if
% more subjects need debugging
%
% fixme: this doesn't work properly for multi_debugs
%
options_append_option_process( debug, All, Pname, DbgMod, _Args, _Defs, NxtEnh, Enh, _Rem ) :-
    partition( option_name(debug), All, Dbgs, Nxt ),
    Dbgs = [Dbg|_],
    !,
    Dbging =.. [debugging,Pname],
    ( call(Dbging) -> Status = true; Status = false ),
    % ( debugging(Pname) -> Status = true; Status = false ),
    Rst = '$restore'(Pname,debug,Status),
    Enh = [Rst|All],
    NxtEnh = [Rst|Nxt],
    options_append_option_process_debug( Dbg, Pname, DbgMod ).
% next clause states that we shouldn't complain if there is no debug/1,2,3
options_append_option_process( debug, All, _Pname, _DbgMod, _Args, _Defs, Nxt, Enh, _Rem ) :-
    % fixme: add option_append option for strictness here ?
    Nxt = All,
    Enh = All.
options_append_option_process( version, All, Pname, _DbgMod, Args, Defs, NxtEnh, Enh, _Opts ) :-
    ( memberchk(version(V),Defs) -> true; V=null ),
    ( memberchk(version(RetV),Args) ->
        ThisV =.. [Pname,V],
        options_append_option_process_version_return( RetV, ThisV ),
        NxtEnh = All, Enh = All
        ;
        % avoid constricting version to an atom, as per defaults option,
        % as this will create problems further on if Opts are passed.
        % we can remove, as user is not going to be able to access these,...
        options_remainder(  All, version, 1, _, Rem ),
        NxtEnh = Rem, Enh = Rem
    ).

options_append_option_process_version_return( RetV, _ThisV ) :-
    ground(RetV),
    !,
    throw( options_ground_return_version(RetV) ).
options_append_option_process_version_return( RetV, ThisV ) :-
    var( RetV ),
    !,
    RetV = [ThisV|_].
options_append_option_process_version_return( [H|T], ThisV ) :-
    options_append_option_process_version_return_list( [H|T], ThisV ),
    !.
options_append_option_process_version_return( Etc, _ThisV ) :-
    throw( options_ground_return_unrecognised_term(Etc) ).

options_append_option_process_version_return_list( Var, ThisV ) :-
    var( Var ),
    !,
    Var = [ThisV|_].
options_append_option_process_version_return_list( [_H|T], ThisV ) :-
    options_append_option_process_version_return_list( T, ThisV ).

options_append_option_process_debug( debug, Pname, Mod ) :-
    options_append_option_process_debug_register( Pname ),
    options_append_option_process_debug_arg( Pname, Mod, true ).
options_append_option_process_debug( debug(DbgS), Pname, Mod ) :-
    options_en_list( DbgS, Dbgs ),
    options_append_option_process_debug_register( Pname ),
    maplist( options_append_option_process_debug_arg(Pname,Mod), Dbgs ).
options_append_option_process_debug( debug(DbgS,Prior), Pname, Mod ) :-  % fixme, this is now dealt by '$restore' ?
    options_en_list( DbgS, Dbgs ),
    options_append_option_process_debug_register( Pname ),
    debugging_status( Pname, Prior ),
    maplist( options_append_option_process_debug_arg(Pname,Mod), Dbgs ).

options_append_option_process_debug_register( Pname ) :-
    ( prolog_debug:debugging(Pname,_,_) -> true; assert(prolog_debug:debugging(Pname,false,[user])) ).

options_append_option_process_debug_arg( _, _, Var ) :-
    var( Var ),
    !,
    throw( options_append(instantiation_of_debug) ).
/*
options_append_option_process_debug_arg( Pname, _ ) :- !,
    \+ prolog_debug:debug_topic(Pname).
    */
options_append_option_process_debug_arg( _Pname, _Mod, none ) :- !.
options_append_option_process_debug_arg( Pname, _Mod, false ) :- !,
    nodebug( Pname ).
options_append_option_process_debug_arg( Pname, Mod, true ) :- !,
    ( Mod == false -> Pterm = Pname; Pterm =.. [Mod,Pname] ),
    ( prolog_debug:debug_topic(Pterm) -> true; prolog_debug:debug_topic(Pterm) ),
    debug( options_append, 'Turning debugging on for predicate handle: ~w', [Pterm] ),
    debug( Pterm ).
options_append_option_process_debug_arg( _Pname, Mod, all ) :- !,
    ( Mod == false -> Pterm = _ ; Pterm =.. [Mod,_] ),
    debug( options_append, 'Turning debugging on for generic handle: ~w', [Pterm] ),
    debug( Pterm ).
options_append_option_process_debug_arg( _, _Mod, Other ) :- !,
    % fixme: should we enforce Mod here ? seems it better not to...
    debug( options_append, 'Turning debugging on for handle: ~w', [Other] ),
    debug( Other ).

options_def_append( Dname, Pname, Args, ExtArgs, Defs, All ) :-
    predicate_name_defined_in( Pname, Mod ),
    debug( options, 'Defined in: ~w', [Mod:Pname] ),
    member( Arity-Gargs, [1-[DefS],2-[ExtArgs,DefS]] ),
    current_predicate( Mod:Dname/Arity ),
    !,
    % append( Left, [DefS], Args ),
    Goal =.. [Dname|Gargs],
    call( Mod:Goal ),
    options_en_list( DefS, Defs ),
    append( Args, Defs, All ).
options_def_append( Dname, _Pname, Args, ExtArgs, Defs, All ) :-
    current_predicate( Dname/Arity ),
    member( Arity-Gargs, [1-[DefS],2-[ExtArgs,DefS]] ),
    !,
    % append( Left, [DefS], Args ),
    Goal =.. [Dname|Gargs],
    call( Goal ),
    options_en_list( DefS, Defs ),
    append( Args, Defs, All ).
options_def_append( Dname, Pname, Opts, _ExtArgs, [], All ) :-
    Mess = 'Could not locate defaults predicate: ~w, in modules: ~w',
    options_message_report( Mess, [Dname/'1,2',[user,Pname]], warning ),
    All = Opts.

options_append_select_own_debug( Opts, NoDebug, Restore ) :-
    once( select(debug(Dbg),Opts,NoDebug) ),
    memberchk( Dbg, [true,false] ),
    !,
    debugging_status( options_append, Restore ),
    options_append_debug_own( Dbg ).
options_append_select_own_debug( Opts, Opts, true ).

options_append_debug_own( true ) :-
    debug( options_append ).
options_append_debug_own( false ).

options_append_restore_debug_status( true ).
options_append_restore_debug_status( false ) :-
    nodebug( options_append ).

option_name( Name, Term ) :-
    options_compound( Term, Name, _Arity ),
    !.
option_name( Name, Term ) :-
    atomic( Term ),
    Name = Term.

% fixme: needs cut ???
predicate_name_defined_in( Pname, Mod ) :-
    current_module( Mod ),
    current_predicate( Mod:Pname/_).
predicate_name_defined_in( Pname, Mod ) :-
    current_module(In),
    current_predicate(In:Pname/N),
    length(List,N),
    Goal =.. [Pname|List],
    predicate_property(In:Goal,imported_from(Mod)).
