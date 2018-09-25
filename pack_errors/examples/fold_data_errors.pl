/*

Example of user defined pack_errors:message/3.

==
?- [pack(pack_errors(examples/fold_data_errors)].

?- throw( fold_data_insufficient(10,8), pack(mlu) )

?- throw( pack_error(fold_data_insufficient(8,10),mlu:ten_fold/3) ).
ERROR: mlu:ten_fold/3: Insufficient length of data (8) as 10 folds are required
==

@author nicos angelopoulos
@version  0.1 2018/9/25

*/

:- multifile( pack_errors:message/3 ).

pack_errors:message( fold_data_insufficient(Dlen,N) ) -->
    ['Insufficient length of data (~d) as ~d folds are required'-[Dlen,N]].
pack_errors:message( fold_data_residual(Dlen) ) -->
    ['Residual data of length: ~d while splitting folds'-[Dlen]].
