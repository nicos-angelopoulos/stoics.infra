
:- use_module(library(lib)).

:- lib(options).
:- lib(debug_call).

exo_defaults([debug(true),op1(one),op2(two)]).

/** exo(+Opts)

Option reporting example.

Opts
  * debug(Dbg=true)
    informational, progress messages

Examples
==
?- exo.
?- exo([]).
% Predicate: exo/1, option selected: op1(one).
% Predicate: exo/1, with options: [debug(true),op1(one),op2(two)].
% Predicate: exo/1, with options: [$restore(exo,debug,true),$restore(exo,debug,false),debug(true),op1(one),op2(two)].
% Option selected: op2(two).
% Option selected: op2(two) from options: [debug(true),op1(one),op2(two)]
% Arbitrary: two
% Variable: X, value: 2
% Variable: Y, value: 3
% Length for list, ops: 2.
% Starting listing of list: ops
% one
% two
% Ended listing of list: ops
% abc starting enumeration of list: ops
% 1.one
% 2.two
% abc ended enumeration of list: ops
% My message is long.
% Dimensions for matrix,  (mtxo) nR: 4, nC: 3.
% fixme dimensions for matrix,  (mtxo) nR: 4, nC: 3.
% Output (claimed) in (non-existing) directory: res
% After mkdir output in directory: res
% pfx1 finished: exo
% Could not locate input file specified by: file.txt, and extensions: 
% Could not locate write file specified by: file.txt, and extensions: 
% Could not locate write file specified by: nonX_file.txt, and extensions: 
% At 18:24:40 on 7th of Oct 2025 stop task: write on file.
% At 18:24:40 on 7th of Oct 2025 unk task: write on file.
% Starting: my_run
% Pwd at, my_run, is: '/home/nicos/pl/packs/src/debug_call/examples/'
% Continuing with: suv file, as: suv-17.09.26.txg, from non singleton list: [suv-17.09.26.txg,suv-17.09.21.txg]
% Finished: exo
true.
true.
==

@author nicos angelopoulos
@version  0.1 2025/10/07

*/
exo :-
     exo( [] ).
exo( Args ) :-
     Self = exo,
     options_append( Self, Args, Opts ),
     options( op1(Op1), Opts ),
     debuc( Self, option, op1(Op1), pred(exo/1) ),
     debuc( Self, options, Opts, pred(exo/1) ),
     debuc( Self, options, Opts, [pred(exo/1),internal(true)] ),
     options( op2(Op2), Opts ),
     debuc( Self, option, op2(Op2) ),
     debuc( Self, option, op2(Op2), all(Opts) ),
     debuc( Self, 'Arbitrary: ~w', Op2 ),
     X = 2,
     debuc( Self, var, 'X':X ),
     Y = 3,
     debuc( Self, var, 'Y':Y, true ),
     Ops = [Op1,Op2],
     debuc( Self, length, ops/Ops, what ),
     debuc( Self, list, ops/ops/Ops, what ),
     debuc( Self, enum, ops/Ops, prefix(abc) ),
     debuc( Self, info, 'My message is ~w.'/long ),
          % ^ written in Green
     Mtx = [h(a,b,c),r(1,2,3),r(4,5,6),r(7,8,9)],
     debuc( Self, dims, mtxo/Mtx ),
     debuc( Self, dims, mtxo/Mtx, prefix(fixme) ),
     debuc( Self, odir, res ),
     shell( 'mkdir res' ),
     debuc( Self, odir, res, prefix('After mkdir') ),
     shell( 'rmdir res' ),
     debuc( Self, end, true, prefix(pfx1) ),
     File = 'file.txt',
     debuc( Self, input, File, path(abs) ),
     debuc( Self, wrote, File, path(abs) ),
     NoxFile = 'nonX_file.txt',
     debuc( Self, wrote, NoxFile ),
     debuc( Self, task(stop), 'write on file' ),
     debuc( Self, task(unk), 'write on file' ),
     debuc( Self, start, my_run ),
     debuc( Self, pwd, my_run ),
     Etcs = ['suv-17.09.26.txg','suv-17.09.21.txg'], Etc = 'suv-17.09.26.txg',
     debug_call( Self, ns_sel, c(Etc,Etcs,'suv file',true) ),
     debuc( Self, end, true ).
