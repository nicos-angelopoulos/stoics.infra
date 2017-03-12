:- module( stoics_lib, [ 
              stoics_lib/0,            % doc predicate 
              stoics_lib_version/2     % -Vers, -Date
              ]
                ).

:- use_module( library(lib) ).
:- ensure_loaded( '../src/auxil/stoics_lib_module.pl' ).
