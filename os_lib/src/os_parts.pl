/** os_parts( +Parts, -Stem ).
    os_parts( -Parts, +Stem ).
    os_parts( ?Parts, ?Stem, +Opts ).

Construct and deconstruct stems of files from/to their constituents. 

Opts 
  * separator(Sep=Sep)
    defaults to flag filename_separator by preference

  * sep(Sep=Sep)
   shortened separator()

==
?- os_parts( Parts, abc_def ).
Parts = [abc, def].

?- os_parts( Parts, 'abc_def-xyz', sep(-) ).
Parts = [abc_def, xyz].

==
*/
os_parts( Parts, Stem ) :-
	os_parts( Parts, Stem, [] ).

os_parts( Parts, Stem, Opts ) :-
	os_sep( Sep, Opts ),
	atomic_list_concat( Parts, Sep, Stem ).
