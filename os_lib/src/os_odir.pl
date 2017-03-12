/** os_odir( -Odir, +GOpts ).
	
	Select output directory from Opts. 
	The order goes, odir() and then dir().

	GOpts is a list of options, not the usual
	list or (sigle, un listed) opton. 

	This is meant as a helper and not an interface predicate.

Opts 
  * odir(Odir)
    preferred output directory
  * odir(Odir)
    preferred output directory

*/
os_odir( Odir, Opts ) :-
	memberchk( odir(Odir), Opts ),
	!.
os_odir( Odir, Opts ) :-
	options( dir(Odir), Opts ).
