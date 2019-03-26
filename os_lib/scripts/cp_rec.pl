

:- use_module(library(lib)).

:- lib(os_lib).
:- lib(options).

/** cp_rec( +Opts )

Recursively copies files while preserving relative directory structure.
Only creates new directories if the structure contains at least 1 matching file.
Files are matched via os_sel/4. Any Opts that are not of src() or dst() form,
are passed as the second argument to os_sel/4.

Opts
  * src(Src='.') 
    source directory
  * dst(Dst)
    destination directory
  * dbg(Dbg=false)
    use _true_ to turn debugging on

==
% upsh cp_rec src=dir_a dst=dir_b ext=svg

% upsh cp_rec dst=testo ext=png dbg=true
==

@author nicos angelopoulos
@version  0:1 2019/3/26
@tbd add option dry() ?

*/
cp_rec( Opts ) :-
    append( Opts, [src('.')], Opts0 ),
    options( src(Src), Opts0, rem_opts(Opts1) ),
    options( dst(Dst), Opts1, rem_opts(Opts2) ),
    options( dbg(Dbg), Opts2, rem_opts(Opts3) ),
    ( Dbg==true-> debug(cp_rec); true ),
    os_sel( os_files, Opts3,  Sel, [dir(Src),sub(true)] ),
    os_make_path( Dst ),
    maplist( cp_rec_file(Src,Dst), Sel ).

cp_rec_file( Src, Dst, RelF ) :-
    debug( cp_rec, '~w -> ~w', [Src/RelF,Dst/RelF] ),
    os_cast( Dst/RelF, + DstF ),
    os_path( DstD, _, DstF ),
    os_make_path( DstD ),
    os_cp( Src/RelF, Dst/RelF ).
