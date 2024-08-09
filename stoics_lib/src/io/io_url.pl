
:- use_module(library(lists)).   % append/3.
:- use_module(library(process)).

:- lib(options).

io_url_defaults( Defs ) :-
                    Defs = [
                                   debug(false),
                                   flags(Flags),
                                   iface(Ifc),
                                   io_call(false),
                                   tmp_rm(true),
                                   tmp_file(_Tmp)
                    ],
     ( catch(process_create(path(which),[curl],[stdout(null)]),_,fail) -> Ifc=curl; Ifc=wget ),
     ( Ifc == curl -> Flags = ['-s','-L']; Flags = [] ).  % fixme: wget is not yet tested
     

/** io_url(+Opts).

Process a URL by downloading it to a local file either via curl or wget.

This is implementation is incomplete. Tested for =|curl|= with default flags.

Opts
  * debug(Dbg=false)
    informational, progress messages
  * flags(Flags)
    defaults flags depend on 
  * iface(Iface)
    default is =|curl|= if installed in the system, or =|wget|= otherwise
  * io_call(Goal=false)
    goal to call (default =|false|= is special, calls nothing). 
    A=|Goal = X-Call|= Unifies X to Tmp and calls call(Goal), otherwise call(Goal,Tmp) is called.
  * tmp_rm(Rmv=true)
    whether to remove the temporary donwload
  * tmp_file(TmpF)
    location for the temporary file. When none is given tmp_file_stream/3 is used to provide one.
    Provide a free variable if you want the name of the created file.
  * url(Url)
    target URL

Examples
==
?- io_url([]).
==

@author nicos angelopoulos
@version  0.1 2024/08/09
@tbd wget interface

*/

io_url( Args ) :-
     Self = io_url,
     options_append( Self, Args, Opts ),
     options( url(Url), Opts ),
     options( iface(Ifc), Opts ),
     options( flags(Flg), Opts ),
     options( tmp_file(Tmp), Opts ),
     options( debug(Dbg), Opts ),
     io_url_file( Ifc, Url, Flg, Tmp, Dbg ),
     options( io_call(Goal), Opts ),
     io_url_call( Goal, Tmp ),
     options( tmp_rm(Rmv), Opts ),
     io_url_remove( Rmv, Tmp ),
     debuc( Self, end, true ).

io_url_file( Ifc, Url, Flg, Tmp, Dbg ) :-
     ( var(Tmp) -> tmp_file_stream(text,Tmp,Stream), close(Stream); true ),
     append( Flg, ['-o',Tmp,Url], Args ),
     debug( Dbg, '', [process_create(path(Ifc),Args,[])] ),
     process_create( path(Ifc), Args, [] ).

io_url_call( false, _File ) :- !.
io_url_call( X-Goal, File ) :-
     !,
     File = X,
     call(Goal).
io_url_call( Goal, File ) :-
     call( Goal, File ).

io_url_remove( true, File ) :-
     delete_file( File ).
io_url_remove( false, _File ).
