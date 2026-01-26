/** even(+Even).

True iff Even is an even integer.

==
?- even(2).
true.

?- even(3).
false.

?- even(4.0).
ERROR: Type error: `integer' expected, found `4.0' (a float)
ERROR: In:
ERROR:   [13] abs(4.0 mod 2)=:=0
ERROR:   [11] toplevel_call(user:user: ...) at /usr/local/users/nicos/local/git/lib/swipl/boot/toplevel.pl:1519
ERROR: 
ERROR: Note: some frames are missing due to last-call optimization.
ERROR: Re-run your program in debug mode (:- debug.) to get more detail.

?- even(-6).
true.

?-  ( (nth1(N1,[a,b,c,d,e,f,d],d),even(N1)) -> write( first_d_at_even_pos(N1) ); write(no_d_found_at_even_position), nl).
first_d_at_even_pos(4)
N1 = 4.

?- ( (nth1(N1,[0,a,b,c,d,e,f,d],d),even(N1)) -> write( first_d_at_even_pos(N1) ); write(no_d_found_at_even_position), nl).
first_d_at_even_pos(8)
N1 = 8.

?- ( (nth1(N1,[0,a,b,c,d,e,f,1,d],d),even(N1)) -> write( first_d_at_even_pos(N1) ); write(no_d_found_at_even_position), nl).
no_d_found_at_even_position
true.

==
@author nicos angelopoulos
@version  0:1 2026/1/26

*/
even( Even ) :- 
     abs(Even mod 2) =:= 0.
