/** lib_type( +PackOrLib, -Type, -Repo, -Root, -Load ).

  Establish the Type, module name (Repo), Root directory and absolute file name
Load-ing file for a repository (PackOrLib). If there is no file to Load it is set to false.

  A system install pack is recognised as path iff (1) the last term in pack.pl is requires(requires) or requires(lib) and
  (2) Pack/prolog/<Pack>.pl is an existing directory in Root.

==
?- lib_type( lib, Type, Repo, Root, Load ).
Type = pack,
Repo= lib,
Root = '/usr/local/users/nicos/local/git/lib/swipl-7.3.33/pack/lib',
Load = '/usr/local/users/nicos/local/git/lib/swipl-7.3.33/pack/lib/prolog/lib.pl'.

?- lib_type( '~/pl/packs/src/lib', Type, Repo, Root, Load ).
Type = pack,
Repo = lib,
Root = '/home/nicos/pl/packs/src/lib',
Load = '/home/nicos/pl/packs/src/lib/prolog/lib.pl'.

?- absolute_file_name( pack(lib), AbsLib ), lib_type( AbsLib, Type, Repo, Root, Load ).
AbsLib = Root, Root = '/usr/local/users/nicos/local/git/lib/swipl-7.3.33/pack/lib',
Type = pack,
Repo = lib,
Load = '/usr/local/users/nicos/local/git/lib/swipl-7.3.33/pack/lib/prolog/lib.pl'.

?- absolute_file_name( pack(lib), AbsLib ), directory_file_path( AbsLib, src, LibSrc ),
   lib_type( LibSrc, Type, Repo, Root, Load ).

AbsLib = '/usr/local/users/nicos/local/git/lib/swipl-7.3.33/pack/lib',
LibSrc = '/usr/local/users/nicos/local/git/lib/swipl-7.3.33/pack/lib/src',
Type = lib,
Repo= user,
Root = '/home/nicos/pl/packs/src/lib/src',
Load = false.

?- absolute_file_name( pack(lib), AbsLib ), directory_file_path( AbsLib, 'prolog/lib.pl', AbsLibF ),
   lib_type( AbsLibF, Type, Repo, Root, Load ).
AbsLib = Root, Root = '/usr/local/users/na11/local/git/lib/swipl-7.5.12/pack/lib',
AbsLibF = Load, Load = '/usr/local/users/na11/local/git/lib/swipl-7.5.12/pack/lib/prolog/lib.pl',
Type = pack,
Repo = lib.

?- Abs = '/usr/local/users/na11/local/git/lib/swipl-7.5.12/pack/lib/prolog',
   lib_type( Abs, Type, Repo, Root, Load ).

?- ls.
.... optios/ ....

?- lib:lib_type( rel(options), Type, Repo, Root, Load ).
Type = lib,
Repo = user,
Root = '/home/na11/web/sware/packs/options',
Load = false.

==

*/
lib_type( rel(Spc), Type, Repo, Root, Load ) :-
    % expand_file_name( DirSpc, [Dir] ),
    AbsOpts1 = [file_type(directory),file_errors(fail)],
    AbsOpts2 = [access(exist),file_errors(fail)],
    once( (absolute_file_name(Spc,Abs,AbsOpts1);absolute_file_name(Spc,Abs,AbsOpts2)) ),
    !,
    lib_type_dir( Abs, Type, Repo, Root, Load ),
    debug( lib, 'Repo explicitly from dir: ~w, with type: ~w and mod: ~w and abs location: ~p', [Spc,Type,Repo,Abs] ).
lib_type( alias(Spc), Type, Repo, Root, Load ) :-
    % we probably need to commit here, but we delay for the message
    AbsOpts1 = [file_type(directory),file_errors(fail)],
    AbsOpts2 = [access(exist),file_errors(fail)],
    once( (absolute_file_name(Spc,Abs,AbsOpts1);absolute_file_name(Spc,Abs,AbsOpts2)) ),
    !,
    lib_type_dir( Abs, Type, Repo, Root, Load ),
    debug( lib, 'Alias: ~w explicitly typed as: ~w, with mod: ~w and abs location: ~p', [Spc,Type,Repo,Load] ).
lib_type( Pack, Type, Repo, Root, Load ) :-
    AbsDirOpts = [file_type(directory),file_errors(fail)],
    ( absolute_file_name( pack(Pack), Root, AbsDirOpts ) ->
        % move the next few lines to a pred that is clever in locating a load file for a pack ?
        directory_file_path( Root, prolog, PlD ),
        directory_file_path( PlD, Pack, AbsStem ),
        file_name_extension( AbsStem, pl, Load ),
        % absolute_file_name(pack(File),Abs,AbsOpts) ),
        lib_type_required( Root )  % this can fail, but its fine to do so after cut as the caller deals with non-requires packs
        ;
        % this is a cell of a pack...
        AbsCellOpts = [file_type(prolog),file_errors(fail),access(read)],
        absolute_file_name( pack(Pack), Load, AbsCellOpts )
    ),
    exists_file( Load ),
    !,
    Type = pack, Repo = Pack,
    debug( lib, 'Repo typed as pack: ~w, with type: ~w and mod: ~w', [Pack,Type,Repo] ).
lib_type( FileSpc, Type, Repo, Root, Load ) :-
    expand_file_name( FileSpc, [File] ),
    AbsOpts = [access(read),file_errors(fail)],
    absolute_file_name( File, Abs, AbsOpts ),
    lib_type_file( Abs, Type, Repo, Root, Load ),
    !,
    debug( lib, 'Repo typed as file: ~w, with type: ~w and mod: ~w', [FileSpc,Type,Repo] ).
lib_type( AbsDir, Type, Repo, Root, Load ) :-
    absolute_file_name( AbsDir, AbsDir ),
    lib_type_dir( AbsDir, Type, Repo, Root, Load ),
    !,
    debug( lib, 'Absolute location defined repo typed as dir: ~w, with type: ~w and mod: ~w', [AbsDir,Type,Repo] ).
/*
lib_type( Other, _Type, _Rmod, _Root, _Load ) :-
    throw( cannot_establish_lib_type_for(Other) ).
    */

lib_type_file( Abs, Type, Repo, Root, Load ) :-
    directory_file_path( PlD, Base, Abs ),
    directory_file_path( Root, prolog, PlD ),
    file_name_extension( Repo, pl, Base ),
    !,
    Type = pack, Load = Abs.
lib_type_file( Abs, lib, user, Root, Abs ) :-
    directory_file_path( Root, _Base, Abs ).

lib_type_dir( Abs, Type, Repo, Root, Load ) :-
    directory_file_path( _, Repo, Abs ),
    directory_file_path( Abs, prolog, PlD ),
    directory_file_path( PlD, Repo, Stem ),
    file_name_extension( Stem, pl, Load ),
    exists_file( Load ),
    !,
    Type = pack, Abs = Root.
lib_type_dir( Abs, Type, Repo, Root, Load ) :-
    Type = lib, Repo = user, Root = Abs, Load = false.

lib_type_required( Dir ) :-
    directory_file_path( Dir, 'pack.pl', PackPl ),
    exists_file( PackPl ),
    open( PackPl, read, In ),
    lib_type_required_in_stream( term, false, In ).

lib_type_required_in_stream( end_of_file, Term, In ) :-
    !,
    close( In ),
    lib_type_required_eof( Term ).
lib_type_required_in_stream( Last, _Previously, In ) :-
    read( In, Next ),
    lib_type_required_in_stream( Next, Last, In ).

lib_type_required_eof( requires(requires) ).
lib_type_required_eof( requires(lib) ).
