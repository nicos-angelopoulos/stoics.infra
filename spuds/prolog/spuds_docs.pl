:- module( spuds_docs, [ spuds_docs/0,
                         all_sources_visible/1,
				     load_user_file/1,
					spuds_report/1
				 ] ).

:- use_module( library(pldoc/doc_library) ).

:- multifile( prolog:doc_directory/1 ).
:- dynamic( prolog:doc_directory/1 ).

:- dynamic( spuds_debug/1 ).
% :- spuds_debug(true).

% see in server/spuds_daemon.pl on how to start debugging the server.
% we should use levels....
spuds_report( What ) :- 
	debugging( spuds ),
	!,
	open( '/tmp/repo.txt', append, Out ), 
     write( Out, What ), nl( Out ),
	close( Out ).
spuds_report( _What ).

load_verbosity( Silent ) :- % added that post-hoc. cann't remember what it should be...
	debugging( spuds ),
	!, 
	Silent = false.
load_verbosity( true ).


%% all_sources_visible( +Bool ).
% 
% A coarse way to turn on (=|Bool=true|=) or off (=|Bool=false|=) 
% the displaying of source code for each predicate.
% Beware, calling this with =|true|= will make all sources visible.
% The default for the Linux server is set to=|true|=.
% 
all_sources_visible( true ) :-
	!, 
	retractall( prolog:doc_directory(_) ),
	assert( prolog:doc_directory(_) ).
all_sources_visible( false ) :-
	retractall( prolog:doc_directory(_) ).
	
spuds_docs :-
	% current_prolog_flag( verbose_load, Verb ),
	% set_prolog_flag( verbose_load, silent ),
	spuds_report( starting_spuds_docs ),
	private_docs,
	spuds_report( done_private ),
	system_docs, % after privates, that check for os_sub
	spuds_report( done_system ),
	packs_docs,
	spuds_report( done_packs ).
	% set_prolog_flag( verbose_load, Verb ).

load_user_file( DcsF ) :-
	spuds_report( testing_user_file(DcsF) ),
	exists_file( DcsF ),
	spuds_report( file_exists(DcsF) ),
	!,
	consult( DcsF ).
load_user_file( File ) :-
	print_message( informational, spuds(no_priv_file(File)) ).

private_docs :-
	private_docs_files,
	private_docs_dirs,
	private_docs_warn.

private_docs_files :-
	current_predicate( user:prolog_source_file/1 ),
	prolog_source_file(File),
	docs_for_file( File, xref ),
	fail.
private_docs_files.

private_docs_dirs :-
	os_sub_does_not_load,
	!,
	spuds_report( os_sub_does_not_load ).
private_docs_dirs :-
	% member( Mod, [user,spuds_docs] ),
	Mod = spuds_docs,
	spuds_report( mod_for_dirs(Mod) ),
	current_predicate( Mod:prolog_source_directory/1 ),
	spuds_report( defined_in(Mod) ),
	Mod:prolog_source_directory( Dir ),
	spuds_report( doing_dir(Dir) ),
	docs_for_dir( Dir, xref, [] ),
	fail.
private_docs_dirs :-
	current_predicate( user:prolog_source_directory/2 ),
	prolog_source_directory( Dir, SubOpts ),
	docs_for_dir( Dir, xref, SubOpts ),
	fail.
private_docs_dirs.

packs_docs :-
	absolute_file_name( swi(pack), PackDir ),
	directory_files( PackDir, Files ),
	assert( real:r_started(true) ),
	% select( os, Files, NonOsFiles ),
	% maplist( pack_subs_docs(PackDir), [os|NonOsFiles] ).
	spuds_report( pack_dirs(Files) ),
	maplist( pack_subs_docs(PackDir), Files ).

pack_subs_docs( Dir, OsObj ) :-
	directory_file_path( Dir, OsObj, Path ),
	exists_directory( Path ),
	directory_file_path( Path, prolog, PackPlDir ),
	exists_directory( PackPlDir ),
	directory_files( PackPlDir, PackFiles ),
	spuds_report( pack_files(PackFiles) ),
	maplist( pack_files_docs(PackPlDir), PackFiles ),
	!.
pack_subs_docs( _, _ ).

pack_files_docs( _Dir, File ) :-
	% directory_file_path( Dir, File, Path ),
	has_prolog_extension( File, Stem ),
	spuds_report( pack_doc_file(File) ),
	docs_for_file( library(Stem), use_module ),
	!.
pack_files_docs( _Dir, _File ).
	
docs_for_dir( Dir, Meth, OptsIn ) :-
	working_directory( Old, Dir ),
	( is_list(OptsIn) -> Opts = OptsIn; Opts = [OptsIn] ),
	ForFiles = [return(files)|Opts],
	spuds_report( os_sub(Dir) ),
	os_sub( '.', os(Subs), ForFiles ),
	spuds_report( subs(Subs) ),
	include( docs_for_file_if_prolog(Meth,Dir), Subs, _Xrefs ),
	working_directory( _, Old ).

docs_for_file_if_prolog( _Meth, Dir, File  ) :-
	directory_file_path( Dir, File, AbsFile ),
	current_predicate( spuds_docs:file_is_blocked_prolog_source/1 ),
	spuds_docs:file_is_blocked_prolog_source( AbsFile ),
	!.  % forget you saw this file
docs_for_file_if_prolog( Meth, Dir, File ) :-
	directory_file_path( Dir, File, AbsFile ),
	is_prolog_file( AbsFile ),
	docs_for_file( AbsFile, Meth ).

is_prolog_file( File ) :-
	file_name_extension( _, pl, File ).
is_prolog_file( File ) :-
	current_predicate( file_is_prolog_source/1 ),
	file_is_prolog_source( File ).

docs_for_file( File ) :-
	docs_for_file( File, xref ).

docs_for_file( LibIndex, _Method ) :-
	atomic( LibIndex ),
	directory_file_path( _, 'LibIndex.pl', LibIndex ),
	!. 
	% Maybe we should add docs_for_required
docs_for_file( File, Method ) :-
	spuds_report( for_file(File) ),
	% absolute_file_name( File, Abs ),
	% assert( swi_doc_seen(Abs) ),
	assert( swi_doc_seen(File) ),
	% docs_for_file_by_method( Method, Abs ).
	spuds_report( method_file(Method,File) ),
	docs_for_file_by_method( Method, File ).
			% fixme: remove silent(true) to see all warnings  
			% fixme: or/and add setting

docs_for_file_by_method( xref, Abs ) :-
	open( Abs, read, In ), read( In, First ), close( In ),
	spuds_report( of_first(Abs,First) ),
	load_verbosity( Silent ),
	spuds_report( l_verbo(Silent) ),
	( First=(:- module(_,_))  -> 
		% catch( use_module(Abs,[]), _, true )
		spuds_report( load_files(Abs,[silent(Silent)]) ), 
		load_files( Abs, [silent(Silent)] )
		;
		Opts = [comments(store),silent(Silent)],
		spuds_report( xref_source(Abs,Opts) ), 
		( catch( xref_source(Abs,Opts), _, fail ) ->
			true
			;
			spuds_report( file_failure(Abs) )
		)
	),
	spuds_report( done_xrefed(Abs) ).
docs_for_file_by_method( use_module, Abs ) :-
	spuds_report( abs(Abs) ),
	catch( use_module(Abs), _, true ).
	% catch( use_module(Abs,[]), _, true ).

/*  docserver does this nowdays ...?
system_docs :-
	current_predicate( blocked_sys_library/1 ),
	!,
	findall( Blc1, (   pldoc_library:blocked(SlBlc1),
	                   atom_concat('/',Blc1,SlBlc1) ),
				        Blc1s ),
	findall( Blc2, blocked_sys_library(Blc2), Blc2s ),
	append( Blc1s, Blc2s, Blcs ),
	partition( has_prolog_extension, Blcs, BlcFs, BlcDs ),
	Opts = [blocked_dirs(BlcDs),blocked_files(BlcFs)],
	absolute_file_name( swi(library), Dir, [file_type(directory)] ),
	docs_for_dir( Dir, use_module, Opts ).
	*/
system_docs :-
	% catch( doc_load_library, _, true ).
	% doc_load_library.
	true.

os_sub_does_not_load :-
	catch( load_files(library(os_sub),[silent(true)]), _, fail ),
	/*
	( catch( load_files([library(os_sub)],[silent(true)]), _, fail ) -> true
		; file_search_path(swi, Swi),
	       atomic_list_concat( [Swi,pack,os_sub,prolog,os_sub], '/', OsSub ),
		  catch( use_module(OsSub), _, fail )
	),
	*/
	!,
	fail.
os_sub_does_not_load :-
	print_message( warning, spuds(os_sub_not_present) ).

private_docs_warn :-
	\+ current_predicate( user:prolog_source_file/1 ),
	\+ current_predicate( user:prolog_source_directory/1 ),
	\+ current_predicate( user:prolog_source_directory/2 ),
	print_message( informational, spuds(no_priv_docs) ),
	!.
private_docs_warn.  % be quite otherwise

has_prolog_extension( File, Stem ) :-
	file_name_extension( Stem, pl, File ),
	!.
has_prolog_extension( File, Stem ) :-
	file_name_extension( Stem, prolog, File ),
	!.
has_prolog_extension( File, Stem ) :-
	file_name_extension( Stem, yap, File ).

:- multifile prolog:message/3.

prolog:message( spuds(no_priv_docs) ) -->
	[ 'Found no private predicates to publish.', nl,
	  'Define prolog_source_file/1 and/or prolog_source_directory/1,2,', nl,
	  'if you wish to add some.' ].

prolog:message( spuds(no_priv_file(File)) ) -->
	[ 'Found no file with user preferences.' - [], nl,
	  'Create one at ~w if you wish to do so.' - [File] ].

prolog:message( spuds(os_sub_not_present) ) --> 
	[ 'To load documentation via prolog_source_directory/1 we need library(os_sub).', 
	  nl,
	  'It can be installed via ?- pack_install(os_sub).' 
	].


