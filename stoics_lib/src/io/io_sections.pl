
:- use_module(library(readutil)).   % read_line_to_codes/2.

io_sections_defaults( Defs ) :-
    Defs = [   
                include_separator(false),
                process(=),
                process_options(false),
                separator(`//`),
                separator_id(false),
                terminating_separator(true)
    ].

/** io_sections( +File, -Sections, +Opts ).

Read a file to a list of Sections. 

Each section is delimited by a marker line and returned as a list of codes as read-in.

Empty sections are currently ignored and not returned.

Opts 
  * include_separator(Inc=false)
    whether to include to the separating line. (Terminator, is never retruned.)
    See example 2.

  * process(Pgoal= (=))
    Goal to process the Sections before storing

  * process_opts(Popts=false)
    if different to =|false|= pass Sopts to processor Pgoal (as last arg)

  * separator_call(SepCall)
    if given it is used to separate sections, called with 1 argument:
    the current line. If the call succeeds, the line is considered to be a 
    separator line. See example 3.

  * separator_id(Sid=false)
    if an integer is taken to be the Id (increasing with each section, otherwise is assumed to be callable,
    for which when first argument is an integer it is assumed to be a counter which is increased with each
    read section, the section lines themselves are passed as penultimate argument and the Id for the section as
    the last argument

  * separator(Sep=`\\`)
    section separating line, used if SepCall is not present 
    (back compatibility, this is now defined as sep_call(==(Line))
    
  * terminating_separator(Tmn=true)
    whether a terminating separator is required at end of file

==
 ?- write('example 1'), nl.
 ?- io_sections( pack('stoics_lib/examples/sectioned.txt'), Sects, separator(`[term]`) ).
 Sects = [[[97], [98]], [[99], [100]]].

 ?- write('example 2'), nl.
 ?- io_sections( pack('stoics_lib/examples/sectioned.txt'), Sects, [separator(`[term]`),include_separator(true)] ).
    Sects = [[[91, 116, 101, 114, 109, 93], [97], [98]], [[91, 116, 101, 114, 109, 93], [99], [100]]].


 ?- write('example 3'), nl.
 ?- io_sections( pack('stoics_lib/examples/sectioned.txt'), Sects, [separator(`[term]`),separator_id(1)] ).
 Sects = [1-[[97], [98]], 2-[[99], [100]]].

 ?- assert( (by_two(N,_,Id) :- Id is N * 2) ).
 ?- io_sections( pack('stoics_lib/examples/sectioned.txt'), Sects, [separator(`[term]`),separator_id(by_two(1))] ).
 Sects = [2-[[97], [98]], 4-[[99], [100]]].

 ?- write('private example'), nl.
 ?- cd( '/usr/local/users/nicos/work/2015/15.10.05-lmtk3_substrates' ).
 ?- io_sections( 'uniprot_sprot.dat', Sects, process(length) ).

==
@author nicos angelopoulos
@version  0.1 2015/10/05
@version  0.2 2016/02/04
@version  0.3 2021/02/04, added include_separator(Inc), examples, pass file through absolute_file_name/2
@vesion   0.4 2025/12/09, changed behaviour of Sid- added example of new usage

*/
io_sections( File, Sections, ArgS ) :-
     ( is_list(ArgS) -> Args = ArgS; Args = [ArgS] ),
     io_sections_defaults( Defs ),
     append( Args, Defs, Opts ),
     ( memberchk(separator_call(Call),Opts) ->
          true
          ;
          memberchk( separator(Sep), Opts ),
          Call = ==(Sep)
     ),
     memberchk( separator_id(Sid), Opts ),
     memberchk( separator(Sep), Opts ),
     memberchk( process(Proc), Opts ),
     memberchk( process_options(Popts), Opts ),
     memberchk( include_separator(Inc), Opts ),

     absolute_file_name( File, FileAbs ),
     open( FileAbs, read, Stream ),
     read_line_to_codes( Stream, Line ),
     memberchk( terminating_separator(Tmn), Opts ),
     io_sections_stream( Line, Stream, Call, Popts, Proc, Tmn, Sid, Inc, [], Sections ),
     close( Stream ).

io_sections_stream( end_of_file, _Stream, _Call, Popts, Proc, Tmn, Sid, Inc, Acc, Sections ) :-
     !,
     io_sections_end_acc( Tmn, Sid, Inc, Popts, Proc, Acc, Sections ).
io_sections_stream( Line, Stream, Call, Popts, Proc, Tmn, Sid, Inc, Acc, Sections ) :-
     call( Call, Line ),
     !,
     io_accumulator_section( Acc, Sid, Popts, Proc, Sections, Nid, Tections ),
     ( Inc == true ->
          NewAcc = [Line]
          ;
          NewAcc = []
     ),
     read_line_to_codes( Stream, New ),
     io_sections_stream( New, Stream, Call, Popts, Proc, Tmn, Nid, Inc, NewAcc, Tections ).
io_sections_stream( Line, Stream, Call, Popts, Proc, Tmn, Sid, Inc, Acc, Sections ) :-
     read_line_to_codes( Stream, New ),
     io_sections_stream( New, Stream, Call, Popts, Proc, Tmn, Sid, Inc, [Line|Acc], Sections ).

% the first clause is to catch empty ones, particularly the first one when we have Sid=true
% maybe a lookahead would be better
io_accumulator_section( [], Sid, _Popts, _Proc, Sections, Sid, Sections ) :- !.
io_accumulator_section( Acc, Sid, Popts, Proc, Sections, Nid, Tections ) :-
     reverse( Acc, Provisional ),
     optioned_call( Popts, Proc, Provisional, Section ),
     io_section_id( Sid, Section, Nid, Iection ),
     Sections = [Iection|Tections].

io_section_id( false, Section, false, Section ) :- !.
io_section_id( Gid, Section, Nid, Id-Section ) :-
     ( integer(Gid) -> 
          Id is Gid,
          Nid is Id + 1
          ;
          call( Gid, Section, Id ),
          ( (compound(Gid),Gid =.. [Fun,Fst|Rst],integer(Fst)) -> 
               Nxt is Fst + 1,
               Nid =.. [Fun,Nxt|Rst]
               ;
               Nid = Gid,
               call( Gid, Section, Id )
          )
     ).

optioned_call( false, Proc, Provisional, Section ) :-
     !,
     call( Proc, Provisional, Section ).
optioned_call( Popts, Proc, Provisional, Section ) :-
     call( Proc, Provisional, Section, Popts ).

io_sections_end_acc( [], _Inc ) :- !.  % should be Inc == false...
io_sections_end_acc( [_A], true ) :- 
     !.
io_sections_end_acc( Acc, true ) :- 
     % fixme: use pack(pack_errors)
     throw( non_empty_end_accumulator(Acc) ).

io_sections_end_acc( true, _Sid, Inc, _Popts, _Proc, Acc, [] ) :-
     io_sections_end_acc( Acc, Inc ).
io_sections_end_acc( false, Sid, _Inc, Popts, Proc, Acc, Sections ) :-
     io_accumulator_section( Acc, Sid, Popts, Proc, Sections, _NxtId, [] ).
