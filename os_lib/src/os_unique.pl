
os_unique_defaults( Defs ) :- 
     Defs = [ 
                by(date([ye,mo,da,[ho,mi],[se]]) ),
                check(true),
                create(true),
                dir('.'),
                ext(csv),
                max_length(ya,2), 
                min_length(_,2),
                place_token(before),
                sep_sub('.'), % was date_sep('.'),
                sep_token('_'),
                sep_parts('-'),
                type(dir)
            ].

:- lib(stoics_lib:get_date_time/1).
:- lib(stoics_lib:break_nth/4).
:- lib(stoics_lib:n_digits_min/3).

:- lib(os_type_create/2).

os_unique_by_date_test :-
    use_module( library(socket) ),  
     gethostname( Host ),
     ( os_unique( [res,Host], Bname, [] )
      ; os_unique( [res,Host], Bname, [min_length(da,3)] )
      ; os_unique( [res,Host], Bname, [max_length(ya,3)] )
    ),
     write( b_name(Bname) ), nl.

%% os_unique( +TokenS, -Os ).
%% os_unique( +TokenS, -Os, +Opts ).
%  
% Create a unique file or directory named Os by using date or version elements and a Token.
% 
% TokenS can be an atomic Token, or a list of Tokens in which case the Separator option will
% also apply within the token parts.
%  
% The predicate does not only provide the name of Os, it also, by default, creates it. 
%
% Opts: 
%   * by(By=date([ye,mo,da,[ho,mi],[se]])) 
%     how to group: either by date with the default taking YeMoDa first, then  adds HoMi and on  the
%     third attempt adds Seconds. For alterative ways to do dates give the respective date/3 term
%     (date/0 is shorthand for default date pattern). For building unique via version give
%     version(Pfx,Compon,Type,Whc) (version/0 is short for version(v,'',1,int,1)).
%   * check(Check=true)
%     when =|false|=, do not make uniqueness check. A =|false|= value makes this predicate a misnomer, however
%     is useful for geting the Os value in say results directory. See examples.
%   * create(Create=true)
%     by default, Os is created
%   * dir(Dir='.')
%     parent directory
%   * ext(Ext=csv)
%     extension to add to Os iff type is file
%   * max_length(Dp=ye,Ye=2) 
%     max length of date Id (_ye_,_mo_,_da_) if using By=date/n or integer if using By=version/n (Nth component), and an integer
%   * min_length(Id='',Len) 
%     min length of date Id (_ye_,_mo_,_da_) if using By=date/n or integer when By=verions/n (Nth component), and an integer
%   * place_token(Plc=before)
%     or _after_, where to place the token in relation to the date
%   * sep_parts(Psep='-')
%     how to conjoin Token and unique part (was token_sep)
%   * sep_sub(Ssep='.')     
%     inter date component separator (was date_sep)
%   * sep_token(Sep='_')
%     separator to be used in bonding TokenS into Token
%   * type(Type=dir)
%     should the unique entry be a _dir_ectory or a _file_.
% 
% Id and DP can be a free variables in which case they match everything.
%
% When using dates for By and call this twice within a single second there is all the chance it will fail.
%
% ==
% ?- os_unique( res, Dname ).
% Dname = 'res-14.05.22'.
% 
% ?- os_unique( res, Dname, [] ).
% Dname = 'res-14.05.22.10.40'.
% 
% ?- os_unique( res, Dname, [] ).
% Dname = 'res-14.05.22.10.40.46'.
% 
% ?- os_unique( res, Dname, [] ).
% Dname = 'res-14.05.22.10.40.57'.
% 
% ?- os_unique( res, Dname, dir('/tmp') ).
% Dname = 'res-21.02.15/'
% % note: dir is created in /tmp
%
% ?- os_unique( res, Dname, [min_length(_,3)] ).
% Dname = 'res-014.005.022'.
%
% ?- os_unique( res, Dname, [token_sep('+'),sep_sub(':'),place_token(after),type(file)] ).
% Fname = '14:05:22+res.csv'.
% 
% ?- os_unique( res, Dname, [token_sep('+'),sep_sub(':'),place_token(after),type(file)] ).
% Dname = '14:05:22:11:03+res.csv'.
% 
% ?- os_unique( tkn, &(Bname), [type(file),ext(tsv)] ).
% Bname = "tkn-16.02.23.tsv".
% 
% ?- os_unique( tkn, &(Bname), [type(file),ext(tsv)] ).
% Bname = "tkn-16.02.23.16.15.tsv".
% 
% ?- os_unique( res, Dname, [ext(png),by(version),create(false),type(file)] ).
% Dname = 'res-v01.png'.
% 
% ?- os_unique( res, Dname, [ext(png),by(version),create(false),type(file)] ).
% Dname = 'res-v01.png'.
% 
% ?- os_unique( res, Dname, [ext(png),by(version),create(false),type(file)] ).
% Dname = 'res-v01.png'.
% 
% ?- os_unique( res, Dname, by(version) ).
% Dname = 'res-v01'.
% 
% ?- os_unique( res, Dname, by(version) ).
% Dname = 'res-v02'.
% 
% ?- os_unique( res, Dname, by(version) ).
% Dname = 'res-v03'.
% 
% ?- os_unique( res, Dname, [by(version),create(false)] ).
% Dname = 'res-v04'.
% 
% ?- os_unique( res, Dname, [by(version),create(false)] ).
% Dname = 'res-v04'.
% ==
%
% To force a restriction of say uniqueness at the date level:
% ==
% ?- os_unique( res, Here, by(date([ye,mo,da])) ).
% Here = 'res-23.06.01'.
% ?- os_unique( res, Here, by(date([ye,mo,da])) ).
% fail.
% ==
%
% As of version 0.6, you can instead do the above with an error
% ==
% ?- os_unique( res1, Here, check(false) ).
% ?- ls.
% bigs.pl           data/             res1-23.06.01/    
% ?- os_unique( res1, Here, check(false) ).
% ERROR: directory `'res1-23.06.01'' does not exist (File exists)
% ...
% ==
%
% Used to be unique_entry_by_date/n, then unique_by_date/n.
%
% @author nicos angelopoulos
% @version  0.3 2016/2/23
% @version  0.4 2016/9/2   changed name from os_unique_by_date, now it also does versioning
% @version  0.5 2021/2/15  added option dir()
% @version  0.6 2023/6/1   added option check()
%
os_unique( Tkn, Bname ) :-
    os_unique( Tkn, Bname, [] ).
os_unique( Tkn, Bname, InOpts ) :-
     options_append( os_unique, InOpts, Opts ),
     memberchk( by(By), Opts ),
     en_list( Tkn, Tkns ),
     options( sep_token(TSep), Opts ),
     atomic_list_concat( Tkns, TSep, TknConc ),
     options( type(Type), Opts ),
     options( dir(Dir), Opts ),
     options( check(Check), Opts ),
     os_unique_by( By, TknConc, Type, Dir, Check, BnamePrv, Opts ),
     options( create(Create), Opts ),
     os_path( Dir, BnamePrv, +(AbsOs) ),
     os_unique_create( Create, Type, AbsOs ),
     os_cast( BnamePrv, Bname ).

os_unique_by( date, TConc, Type, Dir, Check, Bname, Opts ) :-
     !,
     ByTerm = [ye,mo,da,[ho,mi],[se]],
     construct_unique_base_name_by_date( ByTerm, TConc, Opts, Type, Dir, Check, Bname ).
os_unique_by( date(ByTerm), TConc, Type, Dir, Check, Bname, Opts ) :-
    !,
     construct_unique_base_name_by_date( ByTerm, TConc, Opts, Type, Dir, Check, Bname ).
os_unique_by( version, TConc, UType, Dir, Check, Bname, Opts ) :-
    !,
    os_unique_by( version(v,[],int,1), TConc, UType, Dir, Check, Bname, Opts ).
os_unique_by( version(Pfx,Comps,VType,Whc), TConc, UType, Dir, Check, Bname, Opts ) :-
    options( ext(Ext), Opts ),
    options( sep_sub(SSep), Opts ),
    options( sep_parts(PSep), Opts ),
    options( place_token(PlcTkn), Opts ),
    options( min_length(Whc,MinL), Opts ),
    os_unique_by_version_candidate_component( VType, MinL, Cand ),
    % nth_add( Whc, Comps, Cand, CandComps ),
    Brk is Whc - 1,
    break_nth( Brk, Comps, Lcomps, Rcomps ),
    once( append(Lcomps,[Cand|Rcomps],CandComps) ),
    at_con( CandComps, SSep, CandVers ),
    at_con( [Pfx,CandVers], '', CandPfxd ),
    os_unique_concat( PlcTkn, TConc, PSep, CandPfxd, CandStem ),
    type_ext_full( UType, Ext, CandStem, Bname ),
    ( Check==false; \+ os_exists( Bname, dir(Dir) )),
    !.

os_unique_by_version_candidate_component( int, Min, Atom ) :-
    between( 1, infinite,Int ),
    n_digits_min( Min, Int, Atom ).
% fixme: implement:  Atom = a, b, ..., z, aa, ab, ..., az, .... , zz^n
% os_unique_by_version_candidate_component( atom, Min, Atom ).

construct_unique_base_name_by_date( By, TConc, Opts, Type, Dir, Check, Bname ) :-
     partition( atom, By, ByAtoms, ByLists ),
     get_date_time( Datime ),
     findall( max(Key,Len), (member(max_length(KeyIn,Len),Opts),expand_date_key(KeyIn,Datime,Key)), XLengths ),
     findall( min(Key,Len), (member(min_length(KeyIn,Len),Opts),expand_date_key(KeyIn,Datime,Key)), NLengths ),
     append( XLengths, NLengths, Lengths ),
     findall_date_components( ByAtoms, Lengths, Dcomps ),
     memberchk( ext(Ext), Opts ),
     memberchk( sep_sub(DSep), Opts ),
     atomic_list_concat( Dcomps, DSep, DateBit ),
     memberchk( place_token(PlcTkn), Opts ),
     options( sep_parts(PSep), Opts ),
     os_unique_concat( PlcTkn, TConc, PSep, DateBit, CurrStem ),
     type_ext_full( Type, Ext, CurrStem, Current ),
     keep_to_unique_base_name_by_date( Current, ByLists, Lengths, PlcTkn, PSep, DSep, Type, Dir, Check, Ext, Bname ).

os_unique_create( false, _Type, _Os ).
os_unique_create( true, Type, Os ) :-
    os_type_create( Type, Os ).

type_ext_full( dir, _Ext, Current, Current ).
type_ext_full( file, Ext, Current, Full ) :-
    % file_name_extension( Current, Ext, Full ).
    os_ext( Ext, Current, Full ).

findall_date_components( ByAtoms, Lens, Dcomps ) :-
     get_date_time( Datime ),
     findall(  Dcomp, ( member(ByA,ByAtoms),
                        date_time_value(Key,Datime,Val),
                        number( Val ),
                        sub_atom(Key,0,_,_,ByA),
                        IntVal is integer(Val),
                        procruste( Key, IntVal, Lens, Dcomp )
                      ), 
                         Dcomps ).

os_unique_concat( before, Tkn, Sep, Date, Conc ) :-
    atomic_list_concat( [Tkn,Date], Sep, Conc ).
os_unique_concat( after, Tkn, Sep, Date, Conc ) :-
    atomic_list_concat( [Date,Tkn], Sep, Conc ).

keep_to_unique_base_name_by_date( Current, _ByLists, _Lengths, _Plc, _TSep, _DSep, Type, Dir, Check, _Ext, Bname ) :-
    % fixme: Type -> map to something more generic for os_exists/2 ?
    (Check == false; \+ os_exists( Current, [type(Type),dir(Dir)] )),
    % fixme:
     % \+ exists_file( Current ),
     % \+ exists_directory( Current ),
     !,
     Bname = Current.
keep_to_unique_base_name_by_date( Full, [H|T], Lengths, Plc, TSep, DSep, Type, Dir, Check, Ext, Bname ) :-
     findall_date_components( H, Lengths, Dcomps ),
     type_ext_full( Type, Ext, Current, Full ),
     place_stem_date_fragment( Plc, Current, TSep, DSep, Dcomps, Next ),
     type_ext_full( Type, Ext, Next, NextFull ),
     % atomic_list_concat( [Current|Dcomps], IdSep, Next ),
     % we could introduce another separator here.
     keep_to_unique_base_name_by_date( NextFull, T, Lengths, Plc, TSep, DSep, Type, Dir, Check, Ext, Bname ).

place_stem_date_fragment( before, SoFar, _TSep, DSep, Dcomps, Next ) :-
     atomic_list_concat( [SoFar|Dcomps], DSep, Next ).
place_stem_date_fragment( after, SoFar, TSep, DSep, Dcomps, Next ) :-
    atomic_list_concat( [Dsofar|Rem], TSep, SoFar ),
     atomic_list_concat( [Dsofar|Dcomps], DSep, Dnext ),
    atomic_list_concat( [Dnext|Rem], TSep, Next ).

procruste( Key, Int, Lens, Dcomp ) :-
     number_codes( Int, IntCs ),
     length( IntCs, IntLen ),
     copy_term( Lens, CopyLens ),
     ( memberchk(max(Key,Max),CopyLens) -> 
          ( Max<IntLen ->
              Marg is IntLen - Max,
              break_nth( Marg, IntCs, _, RCs )
              ;
              RCs = IntCs 
          )
          ;
          RCs = IntCs 
     ),
     length( RCs, Rlen ),
     ( memberchk(min(Key,Min),CopyLens) ->
          (Min>Rlen ->
               Fill is Min - Rlen,
               findall( 0'0, between(1,Fill,_), ZCs )
               ;
               ZCs = []
          )
          ;
          ZCs = []
     ),
     append( ZCs, RCs, AllCs ),
     atom_codes( Dcomp, AllCs ).

expand_date_key( _KeyIn, _Datime, Key ) :-
     var(Key),
     !.
expand_date_key( KeyIn, Datime, Key ) :-
     date_time_value( Key, Datime, _Value ),
     sub_atom( Key, 0, _, _, KeyIn ),
     !.
