:- lib( en_list/2 ).
:- lib( expand_spec/2).

% :- op( 400, fx, (/) ).  % see lib(os)

%% locate( +File, +Exts, -Locations ).
%
%  Find the exact Location of File that may have a number of extensions.
%  This should become the standard way to interface locating of reading in files.
%  =|Exts = any/*|=, is a special case where any file with matching extension
%  is returned. This case is slower than the rest.
%
%  As of 0.2 only existing files are located. Predicate throws error if file does not exist.
%
%==
%  locate( xyw, abc, Loc ).
%  ERROR: Unhandled exception: Cannot locate file with specification: xyw and extensions: abc
%  
%  
%==
% @author nicos angelopoulos
% @version  0.2 2014/4/24
%
locate( File, Exts, Location ) :-
	locate( File, Exts, '', Location),
	!.
locate( File, Exts, _Location ) :-
	throw( locate(cannot_locate(File,Exts)) ).
	% print_message( error, locate(cannot_locate(File,Exts)) ).

locate( FileSpec, Exts, DefExt, Location ) :-
	( Exts == *; Exts == any ),
	!,
	expand_spec( FileSpec, Expanded ),
	locate_expanded( Expanded, [], '', AbsFile ),
	file_directory_name( AbsFile, Dir ),
	directory_contents( Dir, DFiles, _, _ ),
	directory_files( Dir, Entries ),
	include( exists_file, Entries, DFiles ),
	file_base_name( AbsFile, Bname ),
	include( loc_prefix_atom(Bname), DFiles, Candidates ),
	locate_in_prefix_candidates( Candidates, Bname, DefExt, LcBname ),
	directory_file_path( Dir, LcBname, Location ).

locate( File, Exts, DefExt, Location ) :-
	en_list( Exts, ExtsL ),
	expand_spec( File, Expanded ),
	locate_expanded( Expanded, ExtsL, DefExt, Location ).

% when file exists:
locate_expanded( Expanded, Exts, _DefExt, Location ) :-
	append( Exts, [''], Extensions ),
	Opts = [  extensions(Extensions), access(read),
	          file_errors(fail), solutions(first)   ], 
	absolute_file_name( Expanded, Location, Opts ).
/* commended out in version 0.2, fail with error if you cannot find it. this is for existing files.
locate_expanded( Expanded, _Exts, DefExt, Location ) :-
	Opts = [ access(none), solutions(all) ], 
	absolute_file_name( Expanded, Stem, Opts ),
	file_directory_name( Stem, Dir ),
	exists_directory( Dir ),
	!,
	file_name_extension( Stem, DefExt, Location ).
	*/

loc_prefix_atom( Pfx, Atom ) :-
	atom_concat( Pfx, _, Atom ).

% avoid adding the same extension twice
locate_in_prefix_candidates( [], File, DefExt, Location ) :-
	file_name_extension( _, DefExt, File ),
	!,
	Location = File.
locate_in_prefix_candidates( [], File, DefExt, Location ) :-
	!,
	file_name_extension( File, DefExt, Location ).
locate_in_prefix_candidates( [H|_T], File, _DefExt, Location ) :-
	file_name_extension( File, _Ext, H ),
	!,
	Location = H.
locate_in_prefix_candidates( [_H|T], File, DefExt, Location ) :-
	locate_in_prefix_candidates( T, File, DefExt, Location ).

:- multifile prolog:message//1.
:- discontiguous message//1.

% this is crucial for allowing cathcable exceptions that print when 
prolog:message(unhandled_exception(locate(Message))) -->
	{ debug( locate, 'Unhandled ~p', Message ) },
	message(Message).

prolog:message(locate(Message)) -->
	message(Message).

message( cannot_locate(File,Exts) ) -->
	[ 'Cannot locate file with specification: ~q and extensions: ~q'-[File,Exts] ].
