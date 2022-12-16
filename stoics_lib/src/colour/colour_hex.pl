/** colour_hex( +Clr, -Hex ).

Convert lexical input Clr to a hex string.

Clr is passed through lexi/2.

Currently passes through any represeantion of a hex and maps
the long or code names from latex_colour/4 (1st and 3rd argument to second argument).

==
?- colour_hex( amber, Hex ).
Hex = "#FFBF00".

?- colour_hex( amberic, Hex ).
ERROR: colour_hex/2: Cannot find colour: amberic
false.

?- colour_hex( '#FFBB00', Hex ).
Hex = "#FFBB00".

?- colour_hex( '#FFBB00wrong', Hex ).
Hex = "#FFBB00wrong".
==

@author nicos angelopoulos
@version  0.1 2022/12/16
@see bio_analytics:bio_volcano_plot/1
@see lexi/2
@tbd do some basic checks if a Hex is given

*/
colour_hex( Clr, Hex ) :-
     lexi( Clr, +ClrAtm ),
     colour_hex_atom( ClrAtm, Hex ).

colour_hex_atom( ClrAtm, Hex ) :-
     atom_concat( '#', _, ClrAtm ),
     !,
     lexi( ClrAtm, &(Hex) ).
colour_hex_atom( ClrAtm, Hex ) :-
     latex_colour( ClrAtm, Hex, _, _ ),
     !.
colour_hex_atom( ClrAtm, Hex ) :-
     latex_colour( _, HexAtm, ClrAtm, _ ),
     !,
     lexi( HexAtm, &(Hex) ).
colour_hex_atom( Clr, _Hex ) :-
     Format = 'colour_hex/2: Cannot find colour: ~w',
     message_report( Format, [Clr], error ),
     fail.
