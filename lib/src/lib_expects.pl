%% lib_expects( +PindS ).
%
% lib_expects/1 is a directive unfolded from lib(expects()).
%
% For each Pind in PindS (list or single predicate specification),
% the predicate does nothing if a matching predicate is defined in memory,
% else it prints a warning.
%
% The raison d'être is to flag upstream loading one from possibly many alternatives
% implementation of a predicate that the current predicate depends on.
% 
% For example, see k_sub_graph/5. There, the native predicate
% depends on sub_graph/3. Irrespective of how graphs are represented k_sub_graph/5
% only needs access to sub_graph/3 to do its job. Therefore the 
% following directive is added to the source of k_sub_graph/5:
%
%==
% :- lib_expects( sub_graph/3 ).
%==
%
%==
% ?- lib_expects( nothing/0 ).
% % Warning: Expected predicate nothing/0, not in memory
% ?- use_module( library(lists) ).
% ?- lib_expects( append/3 ).
% true.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/25
%
lib_expects( PspecS ) :-
	( is_list(PspecS) -> Pspecs = PspecS; Pspecs = [PspecS] ),
	maplist( lib_expects_spec, Pspecs ).

%% lib_expects( +Expects, +Mess ).
%% lib_expects( +Expects, +Mess, +Call ).
%
% lib_expects/2,3 is a directive.
%
% Currently Expects should be of the form lib(Lib).
%
% expects/2 will attempt to load Lib via use_module(library(Lib)).
% If that fails a warning containing Mess will be printed and Call
% will be called. Call usually defines operators that allow
% the rest of the code to compile without errors. 
% 
% The idea is to flag missing components and describe in the message
% which part of a pack will be affected, but do not stop the loading
% as the core functionality of a pack is not affected.
%
% To get the warning messages about missing expected libs, set the flag
% 
% ==
% ?- set_prolog_flag( lib_expects_report, true ).
% ==
% @author nicos angelopoulos
% @version  0.1 2014/1/8
%
lib_expects( Term, Mess ) :-
	lib_expects( Term, Mess, true ).

lib_expects( lib(Lib), Mess, Call ) :-
	!,
	Excp = error(existence_error(source_sink,library(Lib)),_), 
	Handler = lib_expects_library_exception_handler(Mess,Lib,Call),
	prolog_load_context( module, Mod ),
	catch( use_module(Mod:library(Lib)), Excp, Handler ).

lib_expects_library_exception_handler( Mess, Lib, Call ) :-
	current_prolog_flag( lib_expects_report, true ),
	Report = '~w, functionality affected by missing library ~w.',
	lib_message_report( Report, [Mess, Lib], warning ),
	call( Call ).
lib_expects_library_exception_handler( Excp, _Mess, _Lib ) :-
	write( other(Excp) ), nl,
	throw( Excp ).

/*
lib_expects_spec( Pname/Arity ) :-
	functor( Head, Pname, Arity ),
	current_predicate( Head ),
	!.
	*/
lib_expects_spec( Pname/0 ) :-
	lib_expects_module( Mod ),
	predicate_property( Mod:Pname, _ ),
	!.

lib_expects_spec( Pname/Arity ) :-
	% current_predicate( Indi ), % SWI doesn't show built ins here
	functor( Head, Pname, Arity ),
	lib_expects_module( Mod ),
	predicate_property( Mod:Head, _ ),
	!.
	/*
lib_expects_spec( Indi ) :-
	user:current_predicate( Indi ),
	!.
	*/
lib_expects_spec( Indi ) :-
	Mess = 'Expected predicate ~w, not in memory',
	lib_message_report( Mess, [Indi], warning ).

lib_expects_module( Mod ) :-
	once( requires_module_context(Mod) ).
lib_expects_module( user ).
