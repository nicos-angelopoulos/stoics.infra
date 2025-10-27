
:- use_module(library(lib)).

:- lib(options).
:- lib(debug_call).

exo_defaults([debug(true),op1(one),op2(two)]).

/** exo(+Opts)

Reporting examples for all debuc Goals in debug_call/4.

Opts
  * debug(Dbg=true)
    informational, progress messages

Examples
==
?- exo.
?- exo([]).
% Predicate: exo/1 option selected: op1(one).
% Predicate: exo/1 options: [debug(true),op1(one),op2(two)]
% Predicate: exo/1 options: [$restore(exo,debug,true),$restore(exo,debug,false),debug(true),op1(one),op2(two)]
% Full predicate: exo/1 options: [$restore(exo,debug,true),$restore(exo,debug,false),debug(true),op1(one),op2(two)]
% Option selected: op2(two).
% Option selected: op2(two) from options: [debug(true),op1(one),op2(two)]
% For predicate: exo/1 option selected: op2(two) from options: [debug(true),op1(one),op2(two)]
% Arbitrary: two
% Reporting term: termo(a,b,c)
% A term from predicate: exo/1 reporting term (termal): termo(a,b,c)
% Variable: X, value: 2
% In predicate: exo/1 variable: Y, value: 3
% Length for list, ops: 2.
% what length for list, ops: 2.
% what predicate: exo/1 length for list, ops: 2.
% Starting listing of list: ops
% one
% two
% Ended listing of list: ops
% Abc starting enumeration of list: ops
% 1.one
% 2.two
% Abc ended enumeration of list: ops
% Starting enumeration of term: term_abc (func: t)
% 1.a
% 2.b
% 3.c
% Ended enumeration of term: term_abc (func: t)
% My message is long.
% Why predicate: exo/1 my message is long ?
% Dimensions for matrix, mtxo: nR: 4, nC: 3.
% fixme dimensions for matrix, mtxo: nR: 4, nC: 3.
% Output (claimed) in (non-existing) directory: res
% After mkdir predicate: exo/1 output in directory: res
% Input from file: '/home/nicos/pl/packs/private/debug_call/a_file.txt'
% RNA expression predicate: exo/1 input from file: rna.tsv
% Wrote on file: '/home/nicos/pl/packs/private/debug_call/a_file.txt'
% Via predicate: exo/1 wrote on file: a_file.txt
% Could not locate write file specified by: nonX_file.txt, and extensions: 
% At 15:50:4 on 27th of Oct 2025 stop task: write on file.
% At 15:50:4 on 27th of Oct 2025 unk task: write on file.
% Prefixed by predicate: exo/1 at 15:50:4 on 27th of Oct 2025 unk task: write on file.
% Starting: my_run
% From predicate: exo/1 starting: my_run
% Pwd at, my_run, is: '/home/nicos/pl/packs/private/debug_call/'
% While in predicate: exo/1 pwd at, my_run, is: '/home/nicos/pl/packs/private/debug_call/'
% Continuing with: suv file, as: suv-17.09.26.txg, from non singleton list: [suv-17.09.26.txg,suv-17.09.21.txg]
% At predicate: exo/1 continuing with: suv file, as: suv-17.09.26.txg, from non singleton list: [suv-17.09.26.txg,suv-17.09.21.txg]
% Avg: 2, list: [1,2,3]
% By call predicate: exo/1 avg: 2, list: [1,2,3]
% pfx1 finished: exo
% At predicate: exo/1 finished: exo
% Finished: exo
true.
==

@author nicos angelopoulos
@version  0.1 2025/10/07
@version  0.2 2025/10/27

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
     debuc( Self, options, Opts, [pred(exo/1),internal(true),prefix('Full')] ),
     options( op2(Op2), Opts ),
     debuc( Self, option, op2(Op2) ),
     debuc( Self, option, op2(Op2), all(Opts) ),
     debuc( Self, option, op2(Op2), [all(Opts),prefix('For'),pred(Self,1)] ),
     debuc( Self, 'Arbitrary: ~w', Op2 ),
     debuc( Self, term, termo(a,b,c) ),
     debuc( Self, term, termo(a,b,c), [term_name(termal),prefix('A term from'),pred(Self,1)] ),
     X = 2,
     debuc( Self, var, 'X':X ),
     Y = 3,
     debuc( Self, var, 'Y':Y, [prefix('In'),pred(Self,1)] ),
     Ops = [Op1,Op2],
     debuc( Self, length, ops/Ops ),
     debuc( Self, length, ops/Ops, prefix(what) ),
     debuc( Self, length, ops/Ops, [prefix(what),pred(Self,1)] ),
     debuc( Self, list, ops/ops/Ops, true ),
     debuc( Self, enum, ops/Ops, prefix('Abc') ),
     debuc( Self, enum, term_abc/t(a,b,c) ),
     debuc( Self, info, 'My message is ~w.'/long ),
          % ^ written in Green
     debuc( Self, info, 'My message is ~w ?'/long, [pred(Self,1),prefix('Why')] ),
     Mtx = [h(a,b,c),r(1,2,3),r(4,5,6),r(7,8,9)],
     debuc( Self, dims, mtxo/Mtx ),
     debuc( Self, dims, mtxo/Mtx, prefix(fixme) ),
     debuc( Self, odir, res ),
     shell( 'mkdir res' ),
     debuc( Self, odir, res, [prefix('After mkdir'),pred(Self,1)] ),
     shell( 'rmdir res' ),
     File = 'a_file.txt',
     shell( 'touch a_file.txt' ),
     debuc( Self, input, File, path(abs) ),
     RnaF = 'rna.tsv',
     shell( 'touch rna.tsv' ),
     debuc( Self, input, RnaF, [prefix('RNA expression'),pred(Self,1)] ),
     shell( 'rm rna.tsv' ),
     debuc( Self, wrote, File, path(abs) ),
     debuc( Self, wrote, File, [prefix('Via'),pred(Self,1)] ),
     NoxFile = 'nonX_file.txt',
     shell( 'rm a_file.txt' ),
     debuc( Self, wrote, NoxFile ),
     debuc( Self, task(stop), 'write on file' ),
     debuc( Self, task(unk), 'write on file' ),
     debuc( Self, task(unk), 'write on file', [prefix('Prefixed by'),pred(Self,1)] ),
     debuc( Self, start, my_run ),
     debuc( Self, start, my_run, [prefix('From'),pred(Self,1)] ),
     debuc( Self, pwd, my_run ),
     debuc( Self, pwd, my_run, [prefix('While in'),pred(Self,1)] ),
     Etcs = ['suv-17.09.26.txg','suv-17.09.21.txg'], Etc = 'suv-17.09.26.txg',
     debug_call( Self, ns_sel, c(Etc,Etcs,'suv file',true) ),
     debug_call( Self, ns_sel, c(Etc,Etcs,'suv file',true), [prefix('At'),pred(Self,1)] ),
     assert( (list_avg_mess(Vist,Vess,Vrgs) :- length(Vist,Ven), sum_list(Vist,Vum), Vvg is Vum / Ven, Vess = 'Avg: ~w, list: ~w', Vrgs=[Vvg,Vist]) ),
     Nist = [1,2,3],
     debuc( Self, call(list_avg_mess), Nist ), 
     debuc( Self, call(list_avg_mess), Nist, [pred(Self,1),prefix('By call')] ), 
     abolish( list_avg_mess/3 ),
     debuc( Self, end, true, prefix(pfx1) ),
     debuc( Self, end, true, [prefix('At'),pred(Self,1)] ),
     debuc( Self, end, true ).
