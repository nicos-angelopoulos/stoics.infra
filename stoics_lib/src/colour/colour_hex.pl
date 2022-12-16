/** bio_colour_hex( Clr, Hex ).

Convert input Clr to a hex 

Clr is passed through lexi as is &(Hex).

Currently passes through any represeantion of a hex and 
the long or code names from bio_latex_colour/4.

==
==


@author nicos angelopoulos
@version  0.1 2022/12/16
@see bio_volcano_plot/1

*/
colour_hex( Clr, Hex ) :-
     lexi( Clr, +ClrAtm ),
     colour_hex_atom( ClrAtm, Hex ).

colour_hex_atom( ClrAtm, Hex ) :-
     atom_concat( '#', _, ClrAtm ),
     !,
     lexi( ClrAtm, &(Hex) ).
colour_hex_atom( ClrAtm, Hex ) :-
     latex_colour( ClrAtm, HexAtm, _, _ ),
     !,
     lexi( HexAtm, &(Hex) ).
colour_hex_atom( ClrAtm, Hex ) :-
     latex_colour( _, HexAtm, ClrAtm, _ ),
     !,
     lexi( HexAtm, &(Hex) ).
colour_hex_atom( Clr, _Hex ) :-
     Format = 'colour_hex/2: Cannot find colour: ~w',
     message_report( Format, [Clr], error ),
     fail.
