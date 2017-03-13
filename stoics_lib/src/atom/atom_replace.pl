
:- lib( sub_atom/4 ).

%% atom_replace( +Atom, +What, +With, -New ).
%
%  Replace all occurances of What in Atom with With to produce New.
%
atom_replace( Atom, What, With, New ) :-
     sub_atom( Atom, Pfx, Psf, What ),
     !,
     atom_replace( Psf, What, With, NewPsf ),
     atom_concat( Pfx, With, PfxWith ),
     atom_concat( PfxWith, NewPsf, New ).
atom_replace( New, _What, _With, New ).
