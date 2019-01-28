:- lib( os_sep/2 ).
:- lib( os_name/2 ).

os_postfix_defaults( Args, Defs ) :-
    Defs = [ sep(Sep),
             replace(false),
             ignore_post([]),
             ext(_),
             with_ext(_)
            ],
    os_sep( Sep, Args ).

%% os_postfix( -Postfix, +Posted ).
%% os_postfix( +Postfix, +Fname, -Posted ).
%% os_postfix( +Postfix, -Fname, +Posted ).
%% os_postfix( +Postfix, ?Fname, ?Posted, +Opts ).
%% os_postfix( +Postfix, +Opts, ?Fname, ?Posted ).
%
% Append a Postfix atom (or list of postfix atoms) to a filename without touching its file type extension.
% Also works for removing a postfix.
% If Postfix is compound of arity/1 is taken to be an aliased path, in which case the innermost path is extended.
%
% Second argument is allowed to be the options (recognised as such when input is a list)
% so that it can be used in meta-calls.
%
% Opts 
%  * ext(Ext)
%    if Ext is ground is assumed to be the strippable part of Fname. When Ext is a variable
%    the found extension is bound to it. This option is for testing and returning, see with_ext() for setting alternative extensions.
% 
%  * ignore_post(Posts=[])
%    (separated) parts to ignore at end (see ex. below)
% 
%  * with_ext(WithExt)
%    replace extension of Fname with WithExt, if ground (you can pick up the old one using an unbound varible in ext(Ext) above)
% 
%  * postfix(Psfx)
%    alternative way of defining Postfix, only used when Postfix is a variable
% 
%  * replace(Rplc=false)
%    replace relevant part of filename instead of adding new postfix
% 
%  * separator(Sep=Sep)    
%    expansion of sep(Sep) - canonical is sep(Sep)
% 
%  * sep(Sep='_')
%    shortened separator() separator for stem-file parts (see os_sep/2)
%
%==
% ?- os_postfix( abc, library(x.txt), T ).
% T = library(x_abc.txt).
% ?- os_postfix( abc, library(x.txt), T, [separator(-)] ).
% T = library('x-abc.txt').
% ?- os_postfix( abc, x.txt, T, sep(.) ).
% T = x.abc.txt.
% ?- os_postfix( v1, graph_layout.csv, T, [ignore_post(layout)] ).
% T = graph_v1_layout.csv.
% ?- os_postfix( v1, graph_lay_out.csv, T, [ignore_post(layout)] ).
% T = graph_lay_out_v1.csv.
% ?- os_postfix( v1, graph_lay_out.csv, T, ignore_post([lay,out]) ).
% T = graph_v1_lay_out.csv.
% ?- os_postfix( v1, graph_lay_out.csv, T, [ignore_post([out]),replace(true)] ).
% T = graph_v1_out.csv
% ?- maplist( os_postfix(v1,[sep(-)]),[a.csv,b.csv], AB ).
% AB = ['a-v1.csv', 'b-v1.csv'].
% ?- os_postfix( _, library(x.txt), T, postfix(abc) ).
% T = library(x_abc.txt).
% ?- os_postfix( _, "x.txt", T, postfix(abc) ).
% T = "x_abc.txt".
% ?- os_postfix( Psf, abc_def.txt ).
% Psf = def.
% ?- os_postfix( bit, by.csv, ByBit, with_ext(txt) ).
% ByBit = by_bit.txt.
% ?- os_postfix( [by,bit], bit.csv, ByBit, with_ext(txt) ).
% ByBit = bit_by_bit.txt.
%==
%
% @author nicos angelopoulos
% @version  0.2 2014/7/8   changed order of 1&2 make it more suitable to meta calls
% @version  0.3 2014/7/28  added aliased paths and example
% @version  0.4 2014/12/2  added options (separator/1,ignore_post/1,replace/1)
%
os_postfix( Psfx, Posted ) :-
    os_postfix( Psfx, _Fname, Posted, [] ).
os_postfix( Psfx, Fname, Posted ) :-
    os_postfix( Psfx, Fname, Posted, [] ).

os_postfix( Psfx, Args, Fname, Posted ) :-
    ground( Args ),
    is_list(Args),
    !,
    os_postfix_reorder( Psfx, Fname, Posted, Args ).
os_postfix( Psfx, Fname, Posted, Args ) :-
    options_append( os_postfix, Args, Opts ),
    os_postfix_1( Psfx, Fname, Posted, Opts ).

os_postfix_1( Psfx, Fname, Posted, Opts ) :-
    ground( Fname ),
    !,
    os_name( Fname, Ftype ),
    os_postfix( Ftype, Psfx, Fname, Posted, Opts ).
    % os_cast( Fname, +(FnAtm) ),
    % os_postfix_atom( FnAtm, Psfx, PsfFAtm, Opts ),
    % os_cast( Ftype, PsfFAtm, Posted ).
os_postfix_1( Psfx, Fname, Posted, Opts ) :-
    ground( Posted ),
    !,
    os_dir_stem_ext( Dir, PoStem, Ext, Posted ),
    os_sep( Sep, Opts ),
    at_con( Parts, Sep, PoStem ),
    options( ignore_post(Igns), Opts, en_list(true) ),
    append( _Short, Long, Igns ),
    append( Clean, Long, Parts ),      %  two appends always succeed at least once
    os_postfix_parts( Psfx, Sep, PsfxParts ),
    append( Withouts, PsfxParts, Clean ),
    !,
    at_con( Withouts, Sep, Fstem ),
    os_dir_stem_ext( Dir, Fstem, Ext, Fname ).

os_postfix( atom, Psfx, File, Posted, Opts ) :-
    os_postfix_atom( File, Psfx, Pile, Opts ),
    os_cast( atom, Pile, Posted ).
os_postfix( string, Psfx, File, Posted, Opts ) :-
    os_postfix_string( File, Psfx, Pile, Opts ),
    os_cast( string, Pile, Posted ).
os_postfix( slash, Psfx, Fname, Posted, Opts ) :-
    Fname = Dir/File,
    os_postfix_atom( File, Psfx, Pile, Opts ),
    os_cast( slash, Dir/Pile, Posted ).
os_postfix( alias, Psfx, Fname, Posted, Opts ) :-
    Fname =.. [Alias,AArg],
    os_postfix_1( Psfx, AArg, PArg, Opts ),
    Pname =.. [Alias,PArg], 
    os_cast( alias, Pname, Posted ).

os_postfix_parts( Psfx, Sep, PsfxParts ) :-
    ground( Psfx ),
    !,
    at_con( PsfxParts, Sep, Psfx ).    %  postfix might itself contain Sep
os_postfix_parts( Psfx, _Sep, [Psfx] ). % when non-ground

os_postfix_reorder( _Psfx, Fname, _Posted, Opts ) :-
    is_list( Fname ),
    !,
    throw( pack_error(os,os_postfix_lists(Fname,Opts)) ).
os_postfix_reorder( Psfx, Fname, Posted, Args ) :-
    options_append( os_postfix, Args, Opts ),
    os_postfix_1( Psfx, Fname, Posted, Opts ).

os_postfix_atom( File, PostfixPrv, Pile, Opts ) :-
    % fixme: allow all_dots(true) as an option that jumps all extensions ?
    options( ext(Ext), Opts ),
    os_ext( Ext, Stem, File ),
     % file_name_extension( Stem, Ext, File ),
    options( [sep(Sep),replace(Rep)], Opts ),
    ( is_list(PostfixPrv) -> atomic_list_concat(PostfixPrv,Sep,Postfix) ; PostfixPrv = Postfix ),
    os_postfix_stem( Stem, Sep, Rep, Prefix, Suffix, Opts ),
    os_postfix_ground( Postfix, Opts ),
    at_con( [Prefix,Postfix,Suffix], Sep, PoStem ),
    file_name_extension( PoStem, Ext, Xile ),
    os_postfix_with_ext( Xile, Pile, Opts ).

os_postfix_string( File, PostfixPrv, Pile, Opts ) :-
    options( ext(Ext), Opts ),
    os_ext( Ext, Stem, File ),
    options( [sep(Sep),replace(Rep)], Opts ),
    ( is_list(PostfixPrv) -> atomic_list_concat(PostfixPrv,Sep,Postfix) ; PostfixPrv = Postfix ),
    os_postfix_stem( Stem, Sep, Rep, Prefix, Suffix, Opts ),
    os_postfix_ground( Postfix, Opts ),
    at_con( [Prefix,Postfix,Suffix], Sep, PoStem ),
    os_ext( Ext, PoStem, Pile ).

os_postfix_with_ext( Xile, Pile, Opts ) :-
    options( with_ext(WithExt), Opts ),
    holds( ground(WithExt), WithExtB ),
    os_postfix_atom_ext( WithExtB, WithExt, Xile, Pile ).

os_postfix_atom_ext( true, WithExt, Xile, Pile ) :-
    os_ext( _, WithExt, Xile, Pile ).
os_postfix_atom_ext( false, _WithExt, Pile, Pile ).

os_postfix_stem( Stem, Sep, Rep, Prefix, Suffix, Opts ) :-
    atomic_list_concat( StemParts, Sep, Stem ),
    options( ignore_post(IgnS), Opts ),
    en_list( IgnS, Igns ),
    os_postfix_stem_ignore( StemParts, Sep, Rep, Igns, Prefix, Suffix ).

os_postfix_stem_ignore( Parts, Sep, Rep, Igns, Prefix, Suffix ) :-
    Igns \== [],
    append( PrefixParts, Igns, Parts ),
    !,
    os_postfix_stem_replace( Rep, PrefixParts, RemainingParts ),
    at_con( RemainingParts, Sep, Prefix ),
    at_con( Igns, Sep, Suffix ).
os_postfix_stem_ignore( PrefixParts, Sep, Rep, _Igns, Prefix, '' ) :-
    os_postfix_stem_replace( Rep, PrefixParts, RemainingParts ),
    at_con( RemainingParts, Sep, Prefix ).

os_postfix_stem_replace( true, PrefixParts, RemainingParts ) :-
    append( RemainingParts, [_Last], PrefixParts ),
    !.
os_postfix_stem_replace( false, Parts, Parts ).

os_postfix_ground( Postfix, _Opts ) :-
    \+ var( Postfix ),
    !.
os_postfix_ground( Postfix, Opts ) :-
    options( postfix(Postfix), Opts ). % fixme: message will be a bit imprecise if missing
