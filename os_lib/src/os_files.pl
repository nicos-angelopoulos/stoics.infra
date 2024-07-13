%% os_files( -Files ).
%% os_files( -Files, +Opts ).
%
% Collects all files for which os_file(File) or os_file(File,Opts) succeed.<br>
% Opts are passed to os_file/2.
% 
%==
% ?- absolute_file_name( pack(os_lib/src), Abs ), os_files( Files, dir(Abs) ).
% Abs = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/os_lib/src',
% Files = [os_abs.pl, os_base.pl, os_cast.pl, os_cp.pl, os_dir.pl, os_dir_stem_ext.pl, os_errors.pl, os_exists.pl, os_ext.pl|...].
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2016/1/31, this version without ref to lib(os_sub)
% @version  0.2 2018/8/05, added options, dir(Dir) and sub(true), removed os_dir_files/2
% @see os_file/2
%
os_files( Files ) :-
    findall( File, os_file(File), Files ).
os_files( Files, Opts ) :-
    % just so we get the version() option updated:
    options_append( os_file, Opts, _All ),
    findall( File, os_file(File,Opts), Files ).
