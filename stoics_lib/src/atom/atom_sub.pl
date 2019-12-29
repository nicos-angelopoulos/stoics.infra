%% atom_sub( +Part, ?Full ).
%
%  An argument reduction and swap of sub_atom/5.
%
% As of version v0.2, Part can be a term:
%  * pfx(Pfx)
%    requires Pfx to be a prefix of Full (makes predicate deterministic)
%  * psf(Psf)
%    requires Psf to be a postfix of Full (makes predicate deterministic)
% 
%==
%
% ? atom_sub( abc, xabcd ).
% true ;
% false.
% ? atom_sub( pfx(x), xabcd ).
% true.
%
%==
%@author nicos angelopoulos
%@version  0.2 2019/12/29
%@version  0.1 2013/12/19
%@see sub_atom/2 sub_atom/5
% 
atom_sub( pfx(Pfx), Full ) :-
    !,
	sub_atom( Full, 0, _, _, Pfx ).
atom_sub( psf(Psf), Full ) :-
    !,
	sub_atom( Full, _, _, 0, Psf ).
atom_sub( Part, Full ) :-
	sub_atom( Full, _, _, _, Part ).
