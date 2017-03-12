os_remove_defaults( [exists(error),debug(false)] ).
os_remove_sub_debugs( [exists,missing] ). % sorted list

/** os_remove( +File ).
    os_remove( +File, +OptS ).
    os_rm( +File ).
    os_rm( +File, +OptS ).

File will be deleted if it exists.
Extends built-in delete_file/1.
Version 1.2 added Options. Opts could be a single option term, see options_append/4.

Opts
  * exists(Ex=error)  
  when 'true' predicate fails if file is missing, 'error' throws error if file is missing,
  'false' does not check whether file exists

  * debug(Dbg=false) 
    when 'true' report whether deleting or else, when 'exists' report existance only

==
?- os_remove( sk, true ).
ERROR: file `sk' does not exist (No such file or directory)

?- os_remove( sk, debug(true) ).
% Skipping deletion of non-existing file: sk
true.

?- shell( 'touch sk' ).
true.

?- os_remove( sk, debug(true) ).
% Deleting existing file: sk
true.

?- os_remove( sk, debug(true) ).
% Skipping deletion of non-existing file: sk
true.

?- os_remove( sk, [exists(true),debug(true)] ).
% Expected file to exist: sk
% failing.
false.

?- os_remove( sk, exists(true) ).
false.

?- os_remove( sk, [exists(error),debug(true)] ).
% os_remove/2 expected file to exist: sk
ERROR: delete_file/1: file `sk' does not exist (No such file or directory)

?- shell( 'touch sk' ).
true.

?- os_remove( "sk" ).
true.

?- os_remove( sk/what, exists(true) ).
ERROR: delete_file/1: file `'sk/what'' does not exist (No such file or directory)

==

@author Nicos Angelopoulos
@version 0.1 2014/09/10

*/

os_rm( Os ) :-
	os_remove( Os, [] ).
os_rm( Os, Opts ) :-
	os_remove( Os, Opts ).

os_remove( File ) :-
	os_remove( File, [] ).

os_remove( File, OptS ) :-
	options_append( os_remove, OptS, Opts, [] ),
	os_remove_debug( Opts, PrevDbg ),
	os_cast( File, atom, Rmv ),
	os_remove_opts( Rmv, Opts ),
	nodebug(os_remove(_)),
	os_remove_debug_restore( PrevDbg ).

os_remove_opts( File, _Opts ) :-
	exists_file(File),
	!,
	debug( os_remove(exists), 'Deleting existing file: ~p', File ),
	delete_file( File ).
os_remove_opts( File, Opts ) :-
	options( exists(Exists), Opts ),
	os_remove_non_existant( Exists, File, Opts ).

os_remove_non_existant( false, File, _Opts ) :-
	debug( os_remove(missing), 'Skipping deletion of non-existing file: ~p', File ).
os_remove_non_existant( true, File, _Opts ) :-
	debug( os_remove(exists), 'Expected file to exist: ~p', File ),
	debug( os_remove(exists), 'Failing.', [] ),
	fail.
os_remove_non_existant( error, File, _Opts ) :-
	debug( os_remove(exists), 'os_remove/2 expected file to exist: ~p', File ),
	% debug( os_remove(exists), 'Throwing a ball.', [] ),
	% throw( os_remove_non_existing(File) ).
	delete_file( File ). % to get the error

os_remove_debug( Opts, PrevDbgs ) :-
	os_remove_sub_debugs( Subs ),
	findall( Dbg, ( debugging(os_remove(Dbg)), memberchk(Dbg,Subs),
				 nodebug(os_remove(Dbg))
	                ), PrevDbgs ),
	option( debug(DbgVal), Opts ),
	os_remove_debug_on( DbgVal ).

os_remove_debug_on( false ). % do nothing, respect global settings.
os_remove_debug_on( true ) :-
	debug( os_remove(exists) ),
	debug( os_remove(missing) ).
os_remove_debug_on( exists ) :-
	debug( os_remove(exists) ).

os_remove_debug_restore( [] ) :- !.
os_remove_debug_restore( PrvDbgs ) :- % a list of os_remove debugging terms
	findall( Dbg, (memberchk(Dbg,PrvDbgs),debug(os_remove(Dbg))), _ ).
	
