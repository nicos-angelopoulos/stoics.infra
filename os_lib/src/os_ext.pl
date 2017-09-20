%% os_ext( ?Ext, +File ).
%% os_ext( +Ext, +Stem, -File ).
%% os_ext( -Ext, -Stem, +File ).
%% os_ext( ?Ext, +New, +File, -NewFile ).
% 
%  Switch the position of the first two arguments of file_name_extension/3.
%  Ext is the file extension of File.  Provides an arity 2 version of 
%  file_name_extension/3. This is appropriate for going through
%  include/3 to filter files of a kind in a list of filenames.
%  In os_ext/3, Ext is not added if it is already the extension to Stem (new in 0.2).
%
%  New is a replacement of Ext in File that produces NewFile.
%  Contrary to file_name_extension/3, os_ext/3 allows dots in Ext.
%==
%  ?- file_name_extension( X, tar.gz, abc.tar.gz ).
%  false.
%  ?- os_ext( tar.gz, Stem, dir/abc.tar.gz ).
%  Stem = dir/abc
%
%  ?- include( os_ext(pl), ['what.pl',none], Pls ).
%  Pls = ['what.pl'].
%
%  ?- maplist( os_ext(xls,csv), [abc.xls,def.xls], New ).
%  New = [abc.csv, def.csv].
%
%  ?- os_ext( Ext, library(abc.txt) ).
%  Ext = txt.
%
%  ?- os_ext( csv, file.csv, File ).
%  File = file.csv.
%
%  ?- os_ext( csv, a.file.csv, File ).
% File = a.file.csv.
%
% ?- os_ext( X, S, a.file.csv ).
% X = csv,
% S = a.file.
%
% ?- os_ext( Old, new, afile.csv, Afile ).
% Old = csv,
% Afile = afile.new.
%
% ?- os_ext( Old, new, library(afile.csv), Afile ).
% Old = csv,
% Afile = library(afile.new).
%
% ?- os_ext( csv, abc/def, Os ).
% Os = abc/def.csv.
% 
% ?- os_ext( csv, library(def), Os ).
% Os = library(def.csv).
%
% ?- os_ext( csv, /def, Os ).
% Os = /def.csv.
% 
% ?- os_ext( csv, abc/def, Os ).
% Os = abc/def.csv.
% 
% ?- os_ext( csv, "abc/def", Os ).
% Os = "abc/def.csv".
% 
% ?- os_ext( csv, "abc/def", +Os ).
% Os = 'abc/def.csv'.
% 
% ?- os_ext( Ext, Stem, "afile.csv" ).
% Ext = "csv",
% Stem = "afile".
%
% ?- os_ext( srt, 'a.file', 'a.file.srt' ).
% true.
%==
% ?- os_ext( Old, srt, 'a.file', Afile ).
% Old = file,
% Afile = a.srt.
% 
% ?- os_ext( txt, Csv, old.txt, New ).
% ERROR: os:os_ext/4: Ground argument expected at position: 2,  (found: _7644)
% ?- os_ext( txt, Csv, New ).
% ERROR: os:os_ext/3: Ground arguments expected in some of the positions: [[1,2],3], but found:[txt,_8390,_8396]
%==
%
% @author nicos angelopoulos
% @version  0.2 2015/5/18   changed behaviour to not adding Ext if it is already in Stem.
% @version  0.3 2016/2/05   added some os typing, and ground Ext can be multi doted
% @version  0.4 2016/1/04   various teething problems after public release
%
os_ext( Ext, File ) :-
     os_ext( Ext, _Stem, File ).

os_ext( Ext, Stem, Os ) :-
	ground(Os),
	!,
	os_name( Os, Type ),
	os_ext_file( Type, Stem, Ext, Os ).
	/*
	os_ext_stem(Stem,Ext,File),
     file_name_extension( _, Ext, Stem ),
	!,
	File = Stem.
	*/
os_ext( Ext, Stem, Os ) :-
	ground( Ext ),
	ground( Stem ),
	!,
	os_name( Stem, Type ),
	os_ext_stem( Type, Stem, Ext, Os ).
os_ext( Ext, Stem, Os ) :-
	throw( pack_error(os,os_ext/3,arg_ground_pattern([[1,2],3],[Ext,Stem,Os])) ).
	% throw( instantiation_error ).

os_ext_file( atom, Stem, Ext, Os ) :-
	ground(Ext),
	!,
	atomic_list_concat( ExtPartsPrv, '.', Ext ),
	( ExtPartsPrv = [''|ExtParts] -> true; ExtParts = ExtPartsPrv ),
	atomic_list_concat( OsParts, '.', Os ),
	once(append(AStemParts,ExtParts,OsParts)), 
	atomic_list_concat( AStemParts, '.', AStem ),
	os_cast( AStem, atom, Stem ).
os_ext_file( atom, Stem, Ext, Os ) :-  % os_ext( X, S, abc.def.ghi ).
	file_name_extension( AStem, AExt, Os ),
	os_cast( AStem, atom, Stem ),
	os_cast( AExt, atom, Ext ).
%
os_ext_file( string, Stem, Ext, Os ) :-
	atom_string( OsAtom, Os ),
	os_ext_file( atom, StemAtom, ExtAtom, OsAtom ),
	os_cast( StemAtom, string, Stem ),
	os_cast( ExtAtom, string, Ext ).
os_ext_file( slash, Stem, Ext, Path ) :-
	Path = Dir/File,
	ground(Ext),
	!,
	atomic_list_concat( ExtParts, '.', Ext ),
	atomic_list_concat( [AStem|ExtParts], '.', File ),
	% file_name_extension( AStem, Ext, File ),
	os_cast( Dir/AStem, slash, Stem ).
os_ext_file( slash, Stemmed, Ext, Path ) :-
	Path = Dir/File,
	file_name_extension( Stem, Ext, File ),
	Stemmed = Dir/Stem.
%
os_ext_file( alias, Stem, Ext, Os ) :-
	Os =.. [Alias,Rel],
	os_name( Rel, RelType ),
	os_ext_file_alias( RelType, Rel, RelStem, Ext ),
	AStem  =.. [Alias,RelStem],
	os_cast( AStem, alias, Stem ).

os_ext_stem( alias, Stem, Ext, Os ) :-
	Stem =.. [Alias,Rel],
	os_name( Rel, RelType ),
	os_ext_stem_alias( RelType, Rel, Ext, RelExt ),
	AOs =.. [Alias,RelExt],
	os_cast( AOs, alias, Os ).
os_ext_stem( atom, Stem, Ext, Os ) :-
	file_name_extension( Stem, Ext, AOs ),
	os_cast( AOs, atom, Os ).
os_ext_stem( slash, Stem, Ext, Os ) :-
	% Stem = Dir/Bstem,  % fixme: / starting and no intermediate / ???
	( Stem = Dir/Bstem ->
		file_name_extension( Bstem, Ext, AOs ),
		os_cast( Dir/AOs, slash, Os )
		;
		Stem = /Bstem,
		file_name_extension( Bstem, Ext, AOs ),
		os_cast( /AOs, slash, Os )
	).
os_ext_stem( string, Stem, Ext, Os ) :-
	atom_string( StemAtom, Stem ),
	file_name_extension( StemAtom, Ext, AOs ),
	os_cast( AOs, string, Os ).

os_ext_stem_alias( alias, Rel, _Ext, _RelExt ) :-
	!,
	throw( pack_error(os,nested_alias(1,os_path/3,Rel)) ).
os_ext_stem_alias( Other, Stem, Ext, Os ) :-
	os_ext_stem( Other, Stem, Ext, Os ).

os_ext_file_alias( alias, Os, _Stem, _Ext ) :-
	!,
	throw( pack_error(os,nested_alias(1,os_path/3,Os)) ).
os_ext_file_alias( Other, Os, Stem, Ext ) :-
	os_ext_file( Other, Stem, Ext, Os ).

os_ext( _Ext, NewExt, _File, _NewFile ) :-
    \+ ground(NewExt),
    !,
	throw( pack_error(os,os_ext/4,arg_ground(2,NewExt)) ).
os_ext( _Ext, _NewExt, File, _NewFile ) :-
    \+ ground(File),
    !,
	throw( pack_error(os,os_ext/4,arg_ground(3,File)) ).
os_ext( Ext, NewExt, File, NewFile ) :-
	os_ext( Ext, Stem, File ),
	os_ext( NewExt, Stem, NewFile ).
