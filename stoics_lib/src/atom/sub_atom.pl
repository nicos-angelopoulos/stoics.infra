%% sub_atom( +Full, ?Part ).
%
%  Short for sub_atom( Full, _, _, _, Sub ).
% 
%  Succeds multiple times. for +Full, +Part.
%
%  See also atom_sub/2.
%==
% ?- sub_atom( abcde, bc ).
% true ;
% false.
%
% ?- findall( Sub, sub_atom(abc,Sub), Subs ), length( Subs, Len ).
% Subs = ['', a, ab, abc, '', b, bc, '', c|...],
% Len = 10.
%==
%
sub_atom( Full, Part ) :-
	sub_atom( Full, _, _, _, Part ).

%% sub_atom( +Full, ?Pre, ?Post, ?Part ).
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
