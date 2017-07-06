
:- lib( os_odir/2 ).

% should these be library wide defautls
%
os_dir_stem_ext_defaults( Defs ) :-
    Defs = [dir('.'),ext(''),type(file),dir_surpress_ext(true)].

/** os_dir_stem_ext( -Os, +Opts ).

Create an os object (Os) from bits in Opts.

Opts
  * ext(Ext='')
     extension for File
  * dir(Dir='.')
     directory for File (if ODIr is not given)
  * dir_surpress_ext(Dre=true) 
     when true, do not use extension for Os 
  * odir(ODir)
     preferred option when constructing Os
  * stem(Stem)
     stem for Os
  * type(Type=file)
     or dir for directory (type only matters for handling extension)

==
?- os_dir_stem_ext( Os, [stem(file),ext(csv)] ).
Os = file.csv.

?- os_dir_stem_ext( Os, [stem(some_dir),ext(csv),type(dir)] ).
Os = some_dir.
==

@author nicos angelopoulos
@version  0.1 2016/6/24
@version  0.2 2017/7/6, added type() and dir_surpress_ext()
@see module docs (how to cross-ref?)

*/
os_dir_stem_ext( Os, Args ) :-
	options_append( os_dir_stem_ext, Args, Opts ),
	options( stem(Stem), Opts ),
	options( ext(Ext), Opts ),
    options( type(Type), Opts ),
    options( dir_surpress_ext(Surp), Opts ),
    os_dir_stem_ext_type_ext( Type, Surp, Ext, OsExt ),
	os_odir( Odir, Opts ),
	os_dir_stem_ext( Odir, Stem, OsExt, Os ).

os_dir_stem_ext_type_ext( dir, true, _Ext, OsExt ) :-
    !,
    OsExt = ''.
os_dir_stem_ext_type_ext( _, _, Ext, Ext ).

%% os_dir_stem_ext( +Dir, +Stem, +Ext, -File ).
%% os_dir_stem_ext( -Dir, -Stem, -Ext, +File ).
%
% Construct and deconstruct filename File, from  and to components: 
% directory Dir, stem Stem and extension Ext.
%
%==
% ?- os_dir_stem_ext( data/what, file, csv, File ).
% File = data/what/file.csv.
% 
% ?- os_dir_stem_ext( 'data/what', file, csv, File ).
% File = 'data/what/file.csv'.
% 
% ?- os_dir_stem_ext( library(os), file, csv, File ).
% File = library('os/file.csv').
%
% ?- os_dir_stem_ext( Dir, Stem, Ext, library('os/file.csv') ).
% Dir = '/usr/local/users/nicos/local/git/lib/swipl-7.3.16/library/os',
% Stem = file,
% Ext = csv.
% 
% ?- os_dir_stem_ext( dir, stem, ext, /Os ).
% dir/stem.ext.
%
% ?- os_dir_stem_ext( "data/what", file, csv, File ).
% File = "data/what/file.csv".
%
% ?- os_dir_stem_ext( "data/what", file, csv, +File ).
% File = 'data/what/file.csv'.
%==
%
% Note: there is no quarantee the path to the file or the file itself exist.
% 
% Also File, might a relative or absolute reference depending on Dir's disposition.
% 
% @author nicos angelopoulos
% @version  0.3 2016/2/7
% @see was dir_stem_ext_file/4
%
os_dir_stem_ext( Dir, Stem, Ext, Path ) :-
	\+ ground( Path ),
	!,
	os_ext( Ext, Stem, Bname ),
	os_path( Dir, Bname, Path ).

os_dir_stem_ext( Dir, Stem, Ext, Path ) :-
	ground( Path ),
	os_path( Dir, Bname, Path ),
	os_ext( Ext, Stem, Bname ).
