/** os_mv( +From, +To ).

Move file from From to To. Behaves as unix mv, 
If To is a directory, From is moved into it (keeping From's basename).

==
% mkdir testo_D; touch testo_D/testo1; touch at_root
?- os_mv( testo_D/testo1, testo_D/example1 ).
true
?- os_mv( at_root, testo_D ).

?- ls( testo_D ).
% at_root    example1   
true.

?- 
% halt

άμπελος;src/os% rm testo_D/example1; rm testo_D/at_root
άμπελος;src/os% rmdir testo_D/;

==

@author nicos angelopoulos
@version  0.1 2016/7/
@tbd debugging

*/
os_mv( From, To ) :-
    maplist( os_cast(atom), [From,To], [FromA,ToA] ),
	( os_dir(ToA) ->
		os_base(FromA,BaseA),
		os_path( ToA, BaseA, ToP )
		;
		ToP = ToA
	),
	rename_file( FromA, ToP ).
