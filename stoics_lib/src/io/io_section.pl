
io_section_open_defaults( Defs ) :-
    Defs = [   
                include_separator(false),
                process(=),
                process_options(false),
                separator(`//`),
                separator_id(false),
                terminating_separator(true)
    ].

/** io_section_open(+File, -SetUp, +Opts ).

Open File for reading a section at a time via iteration.

SetUp should be passed to io_section/3.

Takes same options as io_sections/3.


@author nicos angelopoulos
@version  0.1 2025/12/19

*/
io_section_open( File, SetUp, ArgS ) :-
     ( is_list(ArgS) -> Args = ArgS; Args = [ArgS] ),
     io_sections_defaults( Defs ),
     append( Args, Defs, Opts ),
     ( memberchk(separator_call(Sall),Opts) ->
          true
          ;
          % options( separator(Sep), Opts ),
          memberchk( separator(Sep), Opts ),
          Sall = ==(Sep)
     ),
     memberchk( separator_id(Sid), Opts ),
     memberchk( separator(Sep), Opts ),
     memberchk( process(Proc), Opts ),
     memberchk( process_options(Popts), Opts ),
     memberchk( include_separator(Inc), Opts ),
     memberchk( terminating_separator(Tmn), Opts ),
     absolute_file_name( File, FileAbs ),
     open( FileAbs, read, Rtream ),
     SetUp = set_up(Rtream,Sall,Sid,Inc,Proc,Popts,Tmn).

/** io_section_close( +SetUp ) :-

Should be called at end of io_setion/2 calls to wind down the open stream reated in io_section_open/3.

Any SetUp used in a io_section/3 call as input or output will do. 


@author nicos angelopoulos
@version  0.1 2025/12/19

*/
io_section_close( SetUp ) :-
     SetUp = set_up(Rtream,_Sall,_Sid,_Inc,_Proc,_Popts,_Tmn),
     close( Rtream ).

/** io_section( +SetUp, -Section, +GetUp ).

Read a section at a time from stream and options dictated via SetUp.

The SetUp structure is provided by io_section_open/3. GetUp is like SetUp but with Sid possibly updated.

=|Section = end_of_file |= at the end of sections and to eternity, at which point io_section_close/1 should be called.

Unlike io_sections, which ignores empty sections, here they are returned as empty lists.

==
?-  
    stoics_lib:io_section_open(pack('stoics_lib/examples/sectioned.txt'), Sup, separator(`[term]`)),
    stoics_lib:io_section( Sup, Sect1, Cup ), write( s1(Sect1) ), nl,
    stoics_lib:io_section( Cup, Sect2, Tup ), write( s2(Sect2) ), nl,
    stoics_lib:io_section( Tup, Sect3, Fup ), write( s3(Sect3) ), nl,
    stoics_lib:io_section( Fup, Sect4, Gup ), write( s4(Sect4) ), nl,
    stoics_lib:io_section( Gup, Sect5, Hup ), write( s5(Sect5) ), nl,
    stoics_lib:io_section_close(Sup),
    write( done ), nl,
    fail.

s1([])
s2([[97],[98]])
s3([[99],[100]])
s4(end_of_file)
s5(end_of_file)
done

==
@author nicos angelopoulos
@version  0.1 2025/12/19

*/
io_section( SetUp, Section, NetUp ) :-
     SetUp = set_up(Rtream, Sall, Sid, Inc, Proc, Popts, Tmn),
     read_line_to_codes( Rtream, Line ),
     io_section_line( Line, Rtream, Sall, Sid, Inc, Proc, Popts, Tmn, [], Nid, Section ),
     NetUp = set_up(Rtream, Sall, Nid, Inc, Proc, Popts, Tmn).

io_section_line( end_of_file, _Rtream, _Sall, Sid, Inc, Proc, Popts, Tmn, Acc, Sid, Section ) :-
     !,
     io_sections_end_acc_1( Acc, Tmn, Sid, Inc, Popts, Proc, Section ).

io_section_line( Line, _Rtream, Sall, Sid, _Inc, Proc, Popts, _Tmn, Acc, Nid, Section ) :-
     call( Sall, Line ),
     !,
     io_accumulator_section_1( Acc, Sid, Popts, Proc, [Section|[]], Nid, _ ).
io_section_line( Line, Rtream, Sall, Sid, Inc, Proc, Popts, Tmn, Acc, Nid, Section ) :-
     read_line_to_codes( Rtream, New ),
     io_section_line( New, Rtream, Sall, Sid, Inc, Proc, Popts, Tmn, [Line|Acc], Nid, Section ).

io_sections_end_acc_1( [], _Tmn, _Sid, _Inc, _Popts, _Proc, end_of_file ) :- !.
io_sections_end_acc_1( Acc, Tmn, Sid, Inc, Popts, Proc, Section ) :-
     io_sections_end_acc( Tmn, Sid, Inc, Popts, Proc, [Acc], [Section] ).

% io_sections/3 ignores empty, here we return them.
io_accumulator_section_1( [], Sid, _Popts, _Proc, [[]], Sid, _Tail ) :- !.
io_accumulator_section_1( Acc, Sid, Popts, Proc, Sections, Nid, Tail ) :-
     io_accumulator_section( Acc, Sid, Popts, Proc, Sections, Nid, Tail ).
