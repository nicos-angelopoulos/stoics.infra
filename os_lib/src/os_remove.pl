os_remove_defaults( [err(exists),debug(false)] ).
os_remove_sub_debugs( [exists,missing] ). % sorted list

/** os_remove( +File ).
    os_remove( +File, +OptS ).
    os_rm( +File ).
    os_rm( +File, +OptS ).

File will be deleted if it exists.  Extends built-in delete_file/1.<br>
Version 1.2 added Options. Opts could be a single option term, see options_append/4.<br>
Version 1.3 provides better messaging via throw/2.

Opts
  * err(Ex=exists)  
     controls, messaging and execution if file does not exist (via throw/2).
     values: error, test, exists; or use options on_exit(E) message(M)

  * debug(Dbg=false)
     allows informational message printing for deleting and failed operation

==
?- os_remove( sk, true ).
Warning: os:os_rm/2: OS file: sk, does not exist
false.

?- @touch(sk).
true.

?- os_remove( sk, debug(true) ).
% Deleting existing file: sk
true.

?- os_remove( sk, err(test) ).
false.

?- os_remove( sk, err(error) ).
ERROR: os:os_rm/2: OS file: sk, does not exist

?- os_remove( sk, [on_exit(error),message(warning)] ).
Warning: os:os_rm/2: OS file: sk, does not exist

?- os_remove( sk, [on_exit(false),message(informational)] ).
% os:os_rm/2: OS file: sk, does not exist
false.

?- os_remove( sk, [on_exit(fail),message(warning)] ).
Warning: os:os_rm/2: OS file: sk, does not exist
false.

==

@author Nicos Angelopoulos
@version 0.1 2014/09/10
@version 0.2 2018/10/1,    use throw/2

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
	os_cast( atom, File, Rmv ),
	os_remove_opts( Rmv, Opts ),
	nodebug(os_remove(_)),
	os_remove_debug_restore( PrevDbg ).

os_remove_opts( File, _Opts ) :-
	exists_file(File),
	!,
	debug( os_remove(exists), 'Deleting existing file: ~p', File ),
	delete_file( File ).
os_remove_opts( File, Opts ) :-
	debug( os_remove(missing), 'Cannot remove non existing file: ~p', File ),
    throw( os_exists_not(File,file), [os:os_rm/2|Opts] ).

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
