%% sub_atom( +Full, ?Part ).
%
%  Short for sub_atom( Full, _, _, _, Sub ).
% 
%  As per sub_atom/5, it can succeed multiple times, so leaves backtrack points.
%
%  As of v0.2 +,+ modality calls atom_sub/2 which allows Part to be non atomic.
%
%==
% ?- sub_atom( abcde, bc ).
% true ;
% false.
%
% ?- findall( Sub, sub_atom(abc,Sub), Subs ), length( Subs, Len ).
% Subs = ['', a, ab, abc, '', b, bc, '', c|...],
% Len = 10.
%
% ?- sub_atom( full, psf(ul) ).
% false.
% 
% ?- sub_atom( full, psf(ll) ).
% true.
% 
%==
%
% @author  nicos angelopoulos
% @version 0:2
% @see sub_atom/5, atom_sub/2
%
sub_atom( Full, Part ) :-
    compound( Part ),
    ground( Part ),
    !,
    atom_sub( Part, Full ).
sub_atom( Full, Part ) :-
	sub_atom( Full, _, _, _, Part ).

%% sub_atom( +Full, ?Pre, ?Post, ?Part ).
% 
% As sub_atom/5 but without the Length, 3rd, argument.
%
%==
% ?- sub_atom( full, Pre, Post, ul ).
% Pre = f,
% Post = l ;
% false.
% 
% ?- sub_atom( full, f, l, MidBit ).
% MidBit = ul ;
% false.
%
% ?- sub_atom( ab, Pre, Post, Mid ), write(Pre:Mid:Post), nl, fail.
% : : ab
% :a:b
% :ab:
% a: : b
% a:b:
% ab: : 
% 
%==
%
% @tbd sub_atom/3 with options: begins(t/f), ends(t/f), left(Left), right(Right)
%
sub_atom( Full, Pre, Post, Part ) :-
	sub_atom( Full, Bef, Len, Aft, Part ),
	sub_atom( Full, 0, Bef, _, Pre ),
	Off is Len + Bef,
	sub_atom( Full, Off, Aft, 0, Post ).
