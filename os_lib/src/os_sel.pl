
:- lib(options).
:- lib( stoics_lib:atom_sub/2 ).

os_sel_defaults( [dir('.'),sub(false),version(0:1:3)] ).

/** os_sel( +Oses, +PatternS, -Sel ).
    os_sel( +Oses, +PatternS, -Sel, +Opts ).

Select a number of entries from Oses according to PatternS.
Oses can be one of the following tokens: 
   os_files, os_dirs or os_all or a list of Os objects. 
Tokens are expanded to the respective Os entries within the current directory.
PatternS can be a list of 

PatternS, a list of, or one of: 

  * ext(Ext)
   Oses with extension Ext

  * postfix(Psf)
   is a postfix of the stem of Os

  * prefix(Pfx)
   is a prefix of the stem of Os

  * sub(SubAtom)
   sub_atom/5 succeeds on the stem of the Os

Opts 

  * dir(Dir='.')
    directory at which the Oses will be sought

  * sub(Sub=false)
    should sub dirs be recursed (passed to os_files/2 and os_dirs/2)

==
% mkdir /tmp/os_sel; cd /tmp/os_sel; touch a.pl b.txt c.pl abc.txt; mkdir abc_sub
?- os_sel( os_files, ext(pl), Sel, true ).
Sel = [a.pl, c.pl].

?- os_sel( os_files, ext(txt), Sel, true ).
Sel = [b.txt, abc.txt].

?- os_sel( os_all, sub(abc), Sel, true ).
Sel = [abc.txt, abc_sub].

?- working_directory( Old, '..' ), os_sel( os_dirs, os_, Sel, true ), working_directory( _, Old ).
Old = '/tmp/os_sel/',
Sel = [os_sel].
==

@author nicos angelopoulos
@version  0.1 2016/ 8/24
@version  0.2 2016/10/18  changed naked atoms to sub(Sub), added prefix and postfix
@version  0.3 2017/2/8    allow list of patterns 
@version  0.4 2019/3/26   option sub

*/
os_sel( OsesIn, Pat, Sel ) :-
	os_sel( OsesIn, Pat, Sel, [] ).

os_sel( OsesIn, PatS, Sel, Args ) :-
	options_append( os_sel, Args, Opts ),
	os_sel_oses( OsesIn, Oses, Opts ),
    en_list( PatS, Pats ),
    os_sel_patterns( Pats, Oses, Sel ).

os_sel_patterns( [], Oses, Oses ).
os_sel_patterns( [Pat|Ps], Oses, Sel ) :-
	include( os_sel_pattern(Pat), Oses, Remainder ),
    os_sel_patterns( Ps, Remainder, Sel ).

os_sel_pattern( ext(Ext), Os ) :-
	os_ext( Ext, Os ).
os_sel_pattern( sub(Pat), Os ) :-
    os_cast( Os, +OsAtm ),
	os_ext( _Ext, Stem, OsAtm ),
	once( sub_atom(Stem,_,_,_,Pat) ).
os_sel_pattern( prefix(Pfx), Os ) :-
    os_cast( Os, +OsAtm ),
	os_ext( _Ext, Stem, OsAtm ),
	once( sub_atom(Stem,0,_,_,Pfx) ).
os_sel_pattern( postfix(Psf), Os ) :-
	os_ext( _Ext, Stem, Os ),
	once( sub_atom(Stem,_,_,0,Psf) ).

os_sel_oses( os_files, Oses, Opts ) :-
	!,
    os_files( Oses, [stem(rel)|Opts] ).
os_sel_oses( os_dirs, Oses, Opts ) :-
	!,
	os_dirs( Oses, [stem(rel)|Opts] ).
os_sel_oses( os_all, Oses, Opts ) :-
    !,
    os_files( Files, [stem(rel)|Opts] ),
    os_dirs( Dirs, [stem(rel)|Opts] ),
    append( Dirs, Files, Oses ).
os_sel_oses( Other, Oses, _Opts ) :-
	en_list( Other, Oses ).
