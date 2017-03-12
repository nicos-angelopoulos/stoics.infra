/** os_base( +Os, -Bname ).
	
	Like file_base_name/2 but also works for all formats accepted by os_lib

==
?- os_base( abc/foo.txt, Base ).
Base = foo.txt.

?- os_base( Var, Base ).
ERROR: pack(os): Ground argument expected at position: 1 for predicate: os_base/2, but, _G1156 was found

?- os_base( abc(foo.bar), Base ).
ERROR: pack(os): OS entity: abc(foo.bar), looks like aliased but alias does not exist.

?- os_base( library(foo.bar), Base ).
Base = foo.bar.

?- os_base( "abc/foo.txt", Base ).
Base = "foo.txt".

?- os_base( "abc/foo.txt", +Base ).
Base = foo.txt.

==

*/
os_base( Os, Bname ) :-
	ground( Os ),
	!,
	os_path( _, Bname, Os ).
os_base( Os, _Bname ) :-
	throw( pack_error(os,ground_at(1,os_base/2,Os)) ).
