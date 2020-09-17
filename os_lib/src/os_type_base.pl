/** os_type_base( ?Type, ?Base ).

Map detailed Type to Base type. 

=|file,flink,link,any|= map to =|file|= and =|dir,dlink,link,any|= to =|dir|=.
Predicate is deterministic and in the -,- mode it generates =|file|= for both =|any,link|=.

==

?- os_type_base( flink, Base ).
Base = file.

?- os_type_base( file, Base ).
Base = file.

?- cd( pack('os_lib/examples/testo') ).
?- os_exists( dir1/link2, type(base(Base)) ).
Base = file.
==

@author nicos angelopoulos
@version  0:1 2020/09/17
@see os_exists/2
*/
os_type_base( Type, Base ) :-
    once( os_type_base_table(Type,Base) ).

os_type_base_table(file, file).
os_type_base_table(flink, file).
os_type_base_table(link, file).
os_type_base_table(any, file).
os_type_base_table(dir, dir).
os_type_base_table(dlink, dir).
os_type_base_table(link, dir).
os_type_base_table(any, dir).
