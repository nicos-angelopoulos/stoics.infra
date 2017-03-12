%% os_dir( ?Dir ).
%
% True iff Dir is a directory or a link to an existing directory, in the current directory.
% Directories starting with a '.' are not considered.
% Can be used to enumerate all directories.
%
%==
% ?- cd( pack(os_lib) ).
% ?- os_dir( Dir ).
% Dir = doc ;
% Dir = prolog ;
% Dir = src.
% ?- os_dir( & Dir ).
% Dir = "doc" ;
% Dir = "prolog" ;
% Dir = "src".
%==
%
% @author nicos angelopoulos
% @version  0.2 2016/1/31, this version without ref to lib(os_sub)
% 
os_dir( Dir ) :-
	ground( Dir ),
	!,
	os_exists( Dir, type(dlink) ).
os_dir( Dir ) :-
	directory_files( '.', EntriesUno ),
	sort( EntriesUno, Entries ),
	member( Entry, Entries ),
	\+ atom_concat( '.', _, Entry ),
	os_exists( Entry, type(dlink) ),
	os_cast( Entry, Dir ).

%% os_dirs( -Dirs ).
%% os_dir_dirs( +AtDir, -Dirs ).
%
% Collects all directories for which os_dir(Dir) succeeds in directory AtDir. 
% Dirs are sorted.
% When AtDir is missing it is set to '.'.
%
%==
% ?- cd( pack(os_lib) ).
% ?- os_dirs( Dirs ).
%    Dirs = [doc,prolog, src, doc].
%==
% @author nicos angelopoulos
% @version  0.2 2016/1/31, this version without ref to lib(os_sub)
%
os_dirs( Dirs ) :-
	findall( Dir, os_dir(Dir), Dirs ).

os_dir_dirs( Dir, Dirs ) :-
	os_abs( Dir, Abs ),
	working_directory( Old, Abs ),
	findall( ADir, os_dir(ADir), Dirs ),
	working_directory( _, Old ).
