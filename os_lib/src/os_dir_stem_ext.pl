
:- lib( os_odir/2 ).

% should these be library wide defautls
%
os_dir_stem_ext_defaults( [dir('.'),ext('')] ).

/** os_dir_stem_ext( -Os, +Opts ).

Create a os-name (Os) from bits in Opts.

Opts
  * ext(Ext='')
    extension for File
  * dir(Dir='.')
    directory for File (if ODIr is not given
  * odir(ODir)
    preferred option when constructing output File
  * stem(Stem)
    stem for File

==
?- os_dir_stem_ext( Os, [stem(file),ext(csv)] ).
?- 
==

@author nicos angelopoulos
@version  0.1 2016/6/24
@tbd i thought i have implemented this previously....
@see module docs (how to cross-ref?)

*/
os_dir_stem_ext( Os, Args ) :-
	options_append( os_dir_stem_ext, Args, Opts ),
	options( stem(Stem), Opts ),
	options( ext(Ext), Opts ),
	os_odir( Odir, Opts ),
	os_dir_stem_ext( Odir, Stem, Ext, Os ).

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
