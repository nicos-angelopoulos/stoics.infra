/** <module> A medley of library predicates for stoics packs.

This library collects a medley of library predicates used in more than one stoics projects
and which are not yet matured enough to be published as sub-packs.
pack(lib) looks into the LibIndex.pl of this pack in order to locate source files for pack predicates.


---++ Highlights
  * serves a diverse set of predicates that may be loaded from other packs
  * this is a lazy pack, it can be instructed to only load necessary code and not the whole library code base

---++ Installation

To install
==
?- pack_install( stoics_lib ).
==

to load the whole library
==

?- use_module( library(stoics_lib) ).

==

or 
==
?- use_module( library(lib) ).
?- lib(stoics_lib).
==

To only load specific predicates
== 
?- lib( stoics_lib:kv_compose/3 ).
?- kv_compose( [a,b,c], [1,2,3], KVs ).
KVs = [a-1, b-2, c-3].
?- kv_decompose( [a-1,b-2,c-3], Ls, Ns ).
ERROR: Undefined procedure: kv_decompose/3 (DWIM could not correct goal)
?- lib( stoics_lib:kv_decompose/3 ).
?- kv_decompose( [a-1,b-2,c-3], Ls, Ns ).
Ls = [a, b, c],
Ns = [1, 2, 3].

==

---++ Pack info

@author  nicos angelopoulos
@version   0.1 2017/2/20
@version   0.2 2017/3/7
@version   0.3 2017/3/9
@version   0.4 2017/8/8
@version   0.5 2017/8/15
@version   0.6 2017/10/13
@version   1.0 2018/3/18
@version   1.1 2019/4/22
@version   1.2 2020/9/18
@version   1.3 2020/9/18
@version   1.4 2020/9/18
@see http://www.stoics.org.uk/~nicos/sware/stoics_lib

*/


/** stoics_lib.

This pack does not only  provide its predicates via the module definition, but it can also 
be used to load them on demand. The two methods are transparent and its possible to intermingle:

==
?- lib( stoics_lib:kv_compose/3 ).

==

The main idea is to serve a number of diverse predicates that are not 
ready to be released on their own pack can be used without including them in 
each individual pack that requires them. 

If you want to use any of the predicates in your own pack, simply use
make your pack dependendant to pack(lib) and pack(stoics_lib)
by adding the following line to pack.pl 

==
requires(stoics_lib).
==

Altough 
==
requires(lib).
== 
will also work as library(lib) will also install =stoics_lib= the first time it is referenced.


Note that as =stoics_lib= depends on =|pack(lib)|= that pack will also be installed by the package manager.
You can then include code for (example) predicate io_lines/2 by adding the following to your source code.

==
:- use_module( library(lib) ).
:- lib( stoics_lib:io_lines/2 ).
==
or
==
:- use_module( library(lib) ).
:- lib( stoics_lib:io_lines/2 ).
==

Alternatively, you can make your pack only dependendant on =|pack(lib)|= and the first time 
==
?- lib(stoics_lib).
==

is queried, =|pack(lib)|= will interactively install stoics_lib.

To load stoics_lib predicates without reference to the pack name, first load the index with
lib_load_pack_index/2

==
?- lib_load_pack_index( stoics_lib ).
?- lib( kv_decompose/3 ).
?- kv_decompose([a-1,b-2,c-3], Ls, Ns ).
Ls = [a, b, c],
Ns = [1, 2, 3].
==

*/

stoics_lib.

/** stoics_lib_version( Version, Date ).

Version, term of the from Mj:Mn:Fx and Date is date(Year,Month,Day)

stoics_lib_version( 1:5:0, date(2022,12,29) ).
==
?- stoics_lib_version( -V, -D ).
D = 1:5:1,
V = date(2023,1,2).
==

@author nicos angelopoulos
@version 1:5:0, 2022/12/29
@version 1:6:0, 2023/01/02

*/
stoics_lib_version( 1:6:0, date(2023,1,2) ).
