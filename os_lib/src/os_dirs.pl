%% os_dirs( -Dirs ).
%% os_dirs( -Dirs, +Opts ).
%
% Find all directories for which os_dir(Dir) succeeds.<br>
% Opts are passed to os_dir/2.
%
%==
% ?- cd( pack(os_lib) ).
% ?- os_dirs( Dirs ).
%    Dirs = [doc,prolog, src, doc].
%==
% @author nicos angelopoulos
% @version  0.2 2016/1/31, this version without ref to lib(os_sub)
% @version  0.3 2018/8/05, removed os_dir_dirs/2, now these are simply a findall on os_dir/1,2.
% @see os_dir/2
%
os_dirs( Dirs ) :-
	findall( Dir, os_dir(Dir), Dirs ).
os_dirs( Dirs, Opts ) :-
    % just so we get the version() option updated:
    options_append( os_file, Opts, _All ),
	findall( Dir, os_dir(Dir,Opts), Dirs ).
