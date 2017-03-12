/** os_type_create( +Type, +Os ).

Create an object of the type Os.

Type should either be dir or file.

@author nicos angelopoulos
@version  0.1 2016/09/02
@tbd    options for checking/reporting Os already exists.
*/

os_type_create( file, Os ) :-
	open( Os, write, Out ), close( Out ).

os_type_create( dir, Os ) :-
	make_directory( Os ).
