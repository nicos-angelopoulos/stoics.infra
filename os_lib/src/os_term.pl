%% os_term( +Atom, -Term ).
%% os_term( -Atom, +Term ).
%
%  Bi-directional convertion between atom and slash-term representations of Os entries.
%  Can also be used to ensure the type.
% 
%==
% ?- os_term( Atom, './abc/edf.g' ).
% Atom = './abc/edf.g/'.
% 
% ?- os_term( Atom, '.'/abc/edf.g ).
% Atom = './abc/edf.g/'.
%
% ?- os_term( 'abc/edf.g', Term ), Term = B / C.
% Term = abc/edf.g,
% B = abc,
% C = edf.g.
% 
% ?- os_term( Abc, /abc/def.txt ).
% Abc = '/abc/def.txt'.
% 
% ?- os_term( Abc, abc/def.txt ).
% Abc = 'abc/def.txt'.
%
% % Can be used to ensure a dir is in atom form: 
% ?- os_term( Atom, 'abc/def' ).
% Atom = 'abc/def'.
% 
% ?- os_term( '/abc/edf.g', Term ), Term = /A/B .
% ? Term = /abc/edf.g,
% A = abc,
% B = edf.g.
%
% ?- os_term( './abc/edf.g', Term ).
% Term = ('.')/abc/edf.g.
%
%==
%
% v0.2 added 
%  * /abc/def (when you have / defined as xf operator
%  * use of directory_file_path/3 
%  * renamed from atom_dir_term/2
%
% @author nicos angelopoulos
% @version  0.2 2014/9/16,
%
os_term( Dir, Term ) :-
     ground( Dir ),
     !,
	os_ground_to_term( Dir, Term ).
os_term( Dir, DirTerm ) :-
     ground( DirTerm ),
	os_term_to_atom( DirTerm, Dir ).
     % dir_term_atom( Term, '', Dir ).
/*
os_term( Dir, DirTerm ) :-
     \+ var(DirTerm),
     DirTerm = Term/Base,
     !,
     dir_term_atom( Term, Base, Dir ).
os_term( Dir, Dir ).
*/

os_term_to_atom( /(B), Dir ) :-
	os_term_to_atom( B, Batom ),
	atom_concat( '/', Batom, Dir ),
	% directory_file_path( B, Acc, Bacc ),
	% directory_file_path( '', Bacc, Dir ),
	!.
os_term_to_atom( A/B, Atom ) :-
     !,
	os_term_to_atom( A, AAtom ),
	os_term_to_atom( B, BAtom ),
	atom_concat( '/', BAtom, SBAtom ),
	atom_concat( AAtom, SBAtom, Atom ).
os_term_to_atom( A, A ).

os_ground_to_term( Full, Term ) :-
	atomic( Full ),
	% directory_file_path( Dir, Sub, Full ),
	os_path( Dir, Base, Full ),
	Full \== Base, % Dir \== '.'
	!,
	% sub_dir_atom_term( Dir, Sub, Term ).
	os_ground_to_term( Dir, Left ),
	os_ground_to_term_glue( Left, Base, Term ).
os_ground_to_term( Atom, Atom ).

os_ground_to_term_glue( '', Base, /Base ) :- !.
os_ground_to_term_glue( Dir, Base, Dir/Base ).

sub_dir_atom_term( '/', Sub, Term ) :-
	% directory_file_path( '/', Sub, Term ),
	Term = '/'(Sub),
	!.
sub_dir_atom_term( Atom, '', Term ) :- 
	!,
	os_ground_to_term( Atom, Term ).
% sub_dir_atom_term( Dir, '', Term ) :-
sub_dir_atom_term( Dir, Sub, DirTerm/Sub ) :-
	os_ground_to_term( Dir, DirTerm ).
