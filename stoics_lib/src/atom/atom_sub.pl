%% atom_sub( ?Part, ?Full ).
%
%  An argument reduction and swap of sub_atom/5.
%
%==
% ? atom_sub( abc, xabcd ).
% true ;
% false.
%==
%@author nicos angelopoulos
%@version  0.1 2013/12/19
% 
atom_sub( Part, Full ) :-
	sub_atom( Full, _, _, _, Part ).
