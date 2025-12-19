
:- use_module(library(lib)).

:- lib(os_lib).
:- lib(options).
:- lib(debug_call).

:- lib(stoics_lib).

head_sections_defaults( Defs ) :-
                              Defs = [ 
                                        debug(true),
                                        help(false),
                                        os_sep('_'),
                                        usage(false)
                                     ].


head_sections_help( Self ) :-
     debuc( Self, 'help goes here', [] ).

head_sections_usage( Self ) :-
     debuc( Self, 'upsh ~w debug=true', [Self] ).

/** head_sections(+Opts).

Create a shorter version of a file containing sections by skimming the first N sections.

Uses io_section/3 and not io_sections/3, so can be used as an example of incrementally working through 
sections of a file.

Opts
  * debug(Dbg=true)
    informational, progress messages
  * ext(Ext)
    extension to use for output (InFile's is used if missing)
  * help(Help=false)
    help messsage and exit
  * in_file(InFile)
    input file
  * nos(Nos)
    number of sections
  * os_sep(Sep='_')
    separator for postfix see os_postfix/3
  * out_separator(Out)
    output section separator (codes list)- needs to be present if separator() is not
  * stem(Stem)
    if one given it is used as the baename of the file (with same ext as InFile)
  * postfix(Psfx='<Sep>f<Nos>')
    if Stem is not given use Psfx to create output file from InFile
  * usage(Usg=false)
    usage message and exit

Also all the options accepted by io_section_open/3. 

Examples
==
?- head_sections( [in_file(pack('stoics_lib/examples/sectioned.txt')),stem(ex_sec_2),nos(2),separator(`[term]`)] ).

% This produces a file with 4 lines, 2 setions, first of which is empty.
==

@author nicos angelopoulos
@version  0.1 2025/12/19

*/

head_sections( Args ) :-
     Self = head_sections,
     options_append( Self, Args, Opts ),
     options( '$oa_cont'(Cont), Opts ),
     head_sections_opts( Cont, Self, Opts ),
     debuc( Self, end, true ).

head_sections_opts( false, _Self, _Opts ).
head_sections_opts( true, Self, Opts ) :-
     options( in_file(InF), Opts ),
     io_section_open( InF, SetUp, Opts ),
     options( nos(Nos), Opts ),
     head_sections_ofile( Ofile, InF, Nos, Opts ),
     open( Ofile, write, Out ),
     ( (integer(Nos), Nos > 0) ->
          true
          ;
          throw( nos_should_be_a_positive_int(Nos) )
     ),
     ( memberchk(out_separator(Osep),Opts) -> 
          true
          ;
          ( memberchk(separator(Osep),Opts) -> 
               true
               ;
               throw(one_of_separator__or__out_separator_should_be_given_in_opts)
          )
     ),
     head_sections_nos( Nos, SetUp, Osep, Out, Self ),
     io_section_close( SetUp ),
     close( Out ),
     debuc( Self, wrote, Ofile ).

head_sections_nos( 0, _SetUp, _Osep, _Out, _Self ) :- !.
head_sections_nos( Nos, SetUp, Osep, Out, Self ) :-
     Nos > 0,
     io_section( SetUp, Sect, SetUp1 ),
     ( Sect == end_of_file -> 
          debuc( Self, 'Run out of input with ~d sections missing', [Nos] ),
          Nxs is 0
          ;
          io_lines( Out, Sect ),
          io_lines( Out, [Osep] ),
          Nxs is Nos - 1
     ),
     head_sections_nos( Nxs, SetUp1, Osep, Out, Self ).

head_sections_ofile( Ofile, InF, Nos, Opts ) :-
     ( memberchk(stem(Stem),Opts) ->
               os_ext( Ixt, InF ),
               os_ext( Ixt, Stem, OfilePrv )
               ;
               ( memberchk(postfix(Psfx),Opts) ->
                    true
                    ;
                    atom_concat( f, Nos, Psfx )
               ),
               options( os_sep(Osp), Opts ),
               os_postfix( Psfx, InF, OfilePrv, sep(Osp) )
     ),
     ( memberchk(ext(Ext),Opts) -> 
               os_ext( _, Ext, OfilePrv, Ofile )
               ;
               OfilePrv = Ofile
     ).
