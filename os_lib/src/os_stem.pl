
os_stem_defaults( Args, [dir(''),ext(''),use_from_dir(UFD)] ) :-
	( (memberchk(dir(_),Args);memberchk(odir(_),Args)) -> UFD = false; UFD = true ).

/** os_stem( -Stem, +File, +Opts ).
os_stem( +Stem, -File, +Opts ).
os_stem( -Stem, -File, +Opts ).
os_stem( -Stem, +Opts ).
    

This predicate is meant to be used for generating the second argument (File).
See os_path/3 if you looking for a more general relation between Stem, File and Dir.
When Stem is a variable, Psfx should be given in Opts below.
In addition, hen os_stem/2 is used, From should be given.

Opts
  * dir(Dir)
	default dir only used if ODir is missing, or to return Dir if File is given

  * odir(ODir=' ')
      output directory

  * ext(Ext=' ')
       extension for File in relation to Stem

  * from(From)
    get stem from From (also need to supply postfix())

  * use_from_dir(UseFromDir=true)
    when both Stem and File are variables and From is given, 
    setting this to false will use the parent of From as the destination
    directory (else, either Odir or Dir is used). The default is true
    except when either Dir or ODir are given in Opts
         
  * postfix(Psfx)
    added to From, 

==
?- os_stem( abc, File, dir(sub) ).
File = 'sub/abc'.

?- os_stem( abc, File, [dir(sub),ext(csv)] ).
File = 'sub/abc.csv'.

?- os_stem( Var, File, [from(abc/foo.bar),postfix(ps)] ).
Var = File, File = abc/foo_ps.

?- os_stem( Stem, File, [from(abc/foo.bar),postfix(ps),dir(doc)] ).
Stem = abc/foo_ps,
File = doc/foo_ps.

?- os_stem( Stem, File, [from(abc/foo.bar),postfix(ps),dir(doc),use_from_dir(true)] ).
Stem = File, File = abc/foo_ps.

?- os_stem( Stem, +(File), [from(abc/foo.bar),postfix(ps),dir(doc)] ).
Stem = abc/foo_ps,
File = 'doc/foo_ps'.

?- os_stem( "abc", File, dir(sub) ).
File = 'sub/abc'.

?- os_stem( "abc", File, dir("sub") ).
File = "sub/abc".

?- os_stem( abc, File, dir("sub") ).

==

*/
os_stem( Stem, Args ) :-
	os_stem( Stem, _, Args ).
os_stem( Stem, File, Args ) :-
	options_append( os_stem, Args, Opts ),
	( memberchk(odir(Odir),Opts) -> true; options(dir(Odir),Opts) ),
	( ground(Stem) ->
		options( ext(Ext), Opts ),
		os_dir_stem_ext( Odir, Stem, Ext, File )
		;
		( ground(File) ->
			os_ext( _Ext, Stem, File )
			;
			options([from(From),postfix(Psf)],Opts),
			os_postfix(Psf,From,Posted),
			os_ext(_,Stem,Posted),
			os_ext(_Ext,Stem,FileFrom),
			os_dir_exchange( FileFrom, File, Opts )
		)
	).

% is this useful for other predicates? 
os_dir_exchange( FileFrom, File, Opts ) :-
	options( use_from_dir(true), Opts ),
	!,
	File = FileFrom.
os_dir_exchange( FileFrom, File, Opts ) :-
	( memberchk(odir(Odir),Opts) -> true; options(dir(Odir),Opts) ),
	os_base( FileFrom, Basename ),
	os_name( FileFrom, Type ), 
	os_path( Odir, Basename, FilePrv ),
	os_cast( FilePrv, Type, File ).

