
:- lib(options).

os_un_zip_defaults( Defs ) :-
	Defs = [
				keep(false),
				on_exists(skip)
	].

/** os_un_zip( +File, ?Stem, +Opts ).
os_un_zip( +File ).
os_un_zip( +File, ?Stem ).

Does a simple and safe job of gunzipping files. 
If uncompressed file exists the predicate skips via default, but other
behaviours can be defined via Opts. 

Opts
  * debug(Dbg=false)
    see options_append/3

  * keep(Keep=false) 
    if true, pass Keep option to 

  * on_exists(OnExists=skip) 
    skip: skips the un_zipping (with debug message), error: throws error
    quiet: skip with no debug message, redo: run the un_zip regardless

==
?- tell( ex.txt ), maplist( writeln, [1,2,3,4,5] ), told, shell( 'gzip ex.txt' ), ls.

?- os_un_zip( ex.txt.gz, Stem1, keep(true) ).
?- os_un_zip( ex.txt.gz, Stem2, on_exists(error) ).
==
Second time should be an error as the gunzipped file already exists.

==
?- tell( ex2.txt ), maplist( writeln, [1,2,3,4,5] ), told, shell( 'gzip ex2.txt' ), ls.
?- os_un_zip( ex2.txt.gz, Stem, [keep(true),debug(true)] ).
% Sending: gunzip(-k,-d,ex2.txt.gz)
Stem = ex2.txt.
% Stem: ex.txt exists, so skipping un_zipping of file: ex.txt.gz
Stem = ex.txt.
==
Second time the debug message is different.

@author nicos angelopoulos
@version  0.1 2014/7/2
@version  0.2 2016/6/20, changed from gunzip/2 to os_un_zip/2, added options
@tbd      deal with .tgz files  (do not need this yet)
@tbd      use different engines ? with auto-recognision from extensions ?
*/

os_un_zip( File ) :-
	os_un_zip( File, _Stem, [] ).

os_un_zip( File, Stem ) :-
	os_un_zip( File, Stem, [] ).

os_un_zip( File, Stem, Args ) :-
	file_name_extension( Stem, gz, File ),
	options_append( os_un_zip, Args, Opts ),
	!,
	options( keep(Keep), Opts ),
	options( on_exists(OnExists), Opts ),
	os_un_zip_stem( OnExists, Keep, Stem, File ).
os_un_zip( File, _Stem, _Args ) :-
	throw( unrecognised_gz_extension_for_file(File) ).

os_un_zip_stem( OnX, Keep, Stem, File ) :-
	exists_file( Stem ),
	!,
	os_un_zip_exists( OnX, Keep, Stem, File ).
os_un_zip_stem( _OnX, Keep, Stem, File ) :-
	os_un_zip_stem( Keep, Stem, File ).

os_un_zip_stem( true, _Stem, File ) :-
	% @ gunzip( -k, -d, File ).
	atomic_list_concat( [gunzip,'-k','-d',File], ' ', Gunzip ),
	debug( os_un_zip, 'Sending: ~w', Gunzip ),
	shell( Gunzip ).
os_un_zip_stem( false, _Stem, File ) :-
	% @ gunzip( -d, File ).
	atomic_list_concat( [gunzip,'-d',File], ' ', Gunzip ),
	debug( os_un_zip, 'Sending: ~w', Gunzip ).

os_un_zip_exists( quiet, _Keep, _Stem, _File ).
os_un_zip_exists( skip, _Keep, Stem, File ) :-
	debug( os_un_zip, 'Stem: ~p exists, so skipping un_zipping of file: ~p', [Stem,File] ).
os_un_zip_exists( error, _Keep, Stem, File ) :-
	throw( gunzipped_form_already_exists(Stem,File) ).
os_un_zip_exists( redo, Keep, Stem, File ) :-
	os_un_zip_stem( Keep, Stem, File ).
