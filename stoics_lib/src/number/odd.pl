/** odd(+Odd).

True iff Odd is an odd integer.

==
?- odd(3).
true.

?- odd(2).
false.

?- odd(3.0).
ERROR: Type error: `integer' expected, found `3.0' (a float)
ERROR: In:
ERROR:   [13] abs(3.0 mod 2)=:=1
ERROR:   [11] toplevel_call(user:user: ...) at /usr/local/users/nicos/local/git/lib/swipl/boot/toplevel.pl:1519
ERROR: 
ERROR: Note: some frames are missing due to last-call optimization.
ERROR: Re-run your program in debug mode (:- debug.) to get more detail.

?- odd(-1).
true.

?-  ( (nth1(N1,[a,b,c,d,e,c],c),odd(N1)) -> write( first_c_at_odd_pos(N1) ); write(no_c_found_at_odd_position), nl).
first_c_at_odd_pos(3)
N1 = 3.

?-  ( (nth1(N1,[0,a,b,c,d,e,c],c),odd(N1)) -> write( first_c_at_odd_pos(N1) ); write(no_c_found_at_odd_position), nl).
first_c_at_odd_pos(7)
N1 = 7.

?-  ( (nth1(N1,[0,a,b,c,d,e,1,c],c),odd(N1)) -> write( first_c_at_odd_pos(N1) ); write(no_c_found_at_odd_position), nl).
no_c_found_at_odd_position
true.

==
@author nicos angelopoulos
@version  0:1 2026/1/26

*/
odd( Odd ) :- 
     abs(Odd mod 2) =:= 1.
