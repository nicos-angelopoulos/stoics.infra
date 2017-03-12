%% os_file( ?File ).
%
% True iff File is a file or a link to an existing file, in the current directory.
% Can be used to enumerate all files.
%
%==
% ?- absolute_file_name( pack(os/src), Abs ), os_dir_files( Abs, Files ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.3.16/pack/os/src',
% Files = [os_repoint.pl, os_unique_by_date.pl, os_make_path.pl, os_term.pl, os_path.pl, os_ext.pl, os_abs.pl, os_slashify.pl, os_base.pl|...].
% ?- cd( pack(os_lib) ).
% ?- os_file( File ).
% File = pack.pl ;
% false.
% ?- os_file( & File ).
% File = "pack.pl";
% false.
%==
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
% 
os_file( File ) :-
	ground( File ),
	!,
	os_exists( File, type(flink) ).
os_file( File ) :-
	directory_files( '.', Entries ),
	member( Entry, Entries ),
	os_exists( Entry, type(flink) ),
	os_cast( Entry, File ).

%% os_files( -Files ).
%% os_dir_files( +Dir, -Files ).
%
% Collects all files for which os_file(File) succeeds in directory Dir. 
% When Dir is missing it is set to '.'.
%
%==
% ?- absolute_file_name( pack(os/src), Abs ), os_dir_files( Abs, Files ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.3.16/pack/os/src',
% Files = [os_repoint.pl, os_unique_by_date.pl, os_make_path.pl, os_term.pl, os_path.pl, os_ext.pl, os_abs.pl, os_slashify.pl, os_base.pl|...].
%==
%
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
%
os_files( Files ) :-
	findall( File, os_file(File), Files ).

os_dir_files( Dir, Files ) :-
	os_cast( Dir, atom, ADir ),
	working_directory( Old, ADir ),
	os_files( Files ),
	working_directory( _, Old ).
