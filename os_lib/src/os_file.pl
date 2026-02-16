os_file_defaults( Defs ) :-
    Defs = [ dir('.'), dots(false),
             read_link(false),
             solutions(single), stem(rel), sub(false), 
             version(0:0:6),
             % type checking
             options_types([read_link-boolean,solutions-oneof([single,findall]),stem-oneof([abs,false,rel])])
             ].

%% os_file( ?File ).
%% os_file( ?File, +Opts ).
%
% True iff File is a file or a link to an existing file, in the current directory.<br>
% Can be used to enumerate all files. The order is via sort/2.
%
% Opts
%   * dir(Dir='.')
%      Directory in which to find File.
%   * dots(Dots=false)
%      Set to =true= if dot starting files are required.<br>
%      Note that '.' and '..' are never returned.
%   * links(Links=true)
%     return links pointing to files
%   * read_link(Rlnk=false)
%     If =true=, return the target of links rather than the links (via read_link/3).<br>
%     This makes most sense when =|Stem==abs|=.
%   * solutions(Sol=single)
%     Alternatively set to =findall= for returning a list of all solutions.
%   * stem(Stem=rel)
%      What stem to add to returned files,<br>
%      rel: relative, abs: absolute, false: no stem
%   * sub(Sub=false)
%      Find files within sub directories when set to =true=
%   * version(V,D)
%      Returns version and publication date
%
%==
% ?- cd(pack('os_lib/examples/testo')).
%
% ?- os_file(File).
% File = file1 ;
%
% ?- os_file( & File ).
% File = "file1".
% 
% ?- os_file(File, sub(true)).
% File = 'dir1/file2' ;
% File = 'dir1/link2' ;
% File = file1.
% 
% ?- os_file(File, dots(true)).
% File = '.dotty1' ;
% File = file1.
% 
%:- absolute_file_name( pack(os_lib), OsDir ), working_directory( Old, OsDir ).
% OsDir = '/usr/local/users/nicos/local/git/lib/swipl-7.7.19/pack/os_lib',
% Old = '/home/nicos/.unison/canonical/sware/nicos/git/github/stoics.infra/'.
% 
% File = pack.pl ;
% false.
% 
% ?- os_file( File, solutions(findall) ).
% File = [pack.pl].
% 
% ?- os_file( File, [solutions(findall),sub(true)] ).
% File = ['doc/Releases.txt', 'doc/html/h1-bg.png', 'doc/html/h2-bg.png', 'doc/html/multi-bg.png', 'doc/html/os.html', 'doc/html/pldoc.css', 'doc/html/priv-bg.png', 'doc/html/pub-bg.png', 'examples/testo/dir1/file2'|...].
% 
% ?- os_file( File, [solutions(single),sub(true)] ).
% File = 'doc/Releases.txt' ;
% File = 'doc/html/h1-bg.png' ;
% File = 'doc/html/h2-bg.png' ;
% File = 'doc/html/multi-bg.png'...
%==
% @author nicos angelopoulos 
% @version  0.1 2016/01/31, this version without ref to lib(os_sub)
% @version  0.2 2018/07/23, added options, dir(Dir) and sub(true)
% @version  0.3 2018/10/01, added option dots(Dots)
% @version  0.4 2018/11/04, added option solutions(Sol)
% @version  0.5 2022/02/05, pass Sol through known/1
% @version  0.6 2023/01/02, reverted passing solutions through known. solutions has types, which catches unknown values
% @version  0.7 2026/02/16, opts: links() & version(0:7:0,date(2026,2.16))
% 
os_file( File ) :-
    os_file( File, [] ).

os_file( File, _Opts ) :-
    % fixme: make it play with Opts
   ground( File ),
   !,
   os_exists( File, type(flink) ).
os_file( File, Args ) :-
    options_append( os_file, Args, Opts ),
    ( memberchk(version(0:7:0,date(2026,2,16)),Opts) -> true; true ),
    Know = [dir(Dir),dots(Dots),links(Links),read_link(RLnk),solutions(Sol),stem(Stem),sub(Sub)],
    options( Know, Opts ),
    absolute_file_name( Dir, Here, [file_type(directory),solutions(first)] ),
    os_lib:os_file_sol(Sol, File, Dir, Here, Here, Links, RLnk, Stem, Dots, Sub).

os_file_sol( single, File, Dir, Top, Here, Links, RLnk, Stem, Dots, Sub ) :-
    os_file( File, '', Dir, Top, Here, Links, RLnk, Stem, Dots, Sub ).
os_file_sol( findall, Files, Dir, Top, Here, Links, RLnk, Stem, Dots, Sub ) :-
    findall( File, os_file(File,'',Dir,Top,Here,Links,RLnk,Stem,Dots,Sub), Files ).

os_file( File, Rel, Dir, Top, Here, Links, RLnk, Stem, Dots, Sub ) :-
    os_cast( Dir, +SysDir ),
    directory_files( SysDir, EntriesUno ),
    sort( EntriesUno, Entries ),
    member( Entry, Entries ),
    Entry \== '.', Entry \== '..',
    os_file_dot( Dots, Entry ),
    os_path( Dir, Entry, Desc ),
    os_path( Rel, Entry, RelDesc ),
    os_file_obj( Desc, RelDesc, Entry, File, Dir, Top, Here, Links, RLnk, Stem, Dots, Sub ).

os_file_dot( true, _Os ).
os_file_dot( false, Os ) :- 
    \+ atom_concat( '.', _, Os ).

os_file_obj( Desc, Rel, Entry, File, _Dir, Top, Here, Links, RLnk, Stem, _Dots, _Sub ) :-
    os_exists( Desc, [type(flink),err(test)] ),
    !,
    os_file_obj_return( RLnk, Stem, Rel, Entry, Top, Here, File ),
    !.
os_file_obj( Desc, Rel, Entry, File, _Dir, Top, Here, RLnk, Stem, Dots, true ) :-
    os_exists( Desc, type(dlink) ),
    os_path( Here, Entry, There ),
    os_file( File, Rel, Desc, Top, There, RLnk, Stem, Dots, true ).

os_file_obj_return( false, false, _Rel, Entry, _Top, _Here, File ) :-
     os_cast( Entry, File ).
os_file_obj_return( false, abs, _Rel, Entry, _Top, Here, File ) :-
     os_path( Here, Entry, Path ),
     os_cast( Path, File ).
os_file_obj_return( false, rel, Rel, _Entry, _Top, _Here, File ) :-
    os_cast( Rel, File ).
os_file_obj_return( true, Stem, Rel, Entry, Top, Here, File ) :-
     ( os_exists(Rel,[type(link),err(test)]) ->
          os_file_obj_return_link( Stem, Rel, Entry, Top, Here, File )
          ;
          % then re-use code for when RLnk is false
          os_file_obj_return( false, Stem, Rel, Entry, Top, Here, File )
     ).

os_file_obj_return_link( false, Rel, _Entry, _Top, _Here, File ) :-
     read_link( Rel, _, Target ),
     % fixme: untested, makes little sense, but warning has been given at docs of Opts
     os_path( Target, _, File ).
os_file_obj_return_link( abs, Rel, _Entry, Top, _Here, File ) :-
     read_link( Rel, _, Target ),
     absolute_file_name( Target, AbsFile, [relative_to(Top)] ),
     os_cast( AbsFile, File ).
os_file_obj_return_link( rel, Rel, _Entry, _Top, _Here, File ) :-
    os_cast( Rel, File ).

os_files_defaults( [dir('.'),sub(false)] ).
