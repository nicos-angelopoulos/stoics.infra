% this file is consulted so you can do quite a bit of intialisation here.
%
% like:
% requires( _ ).  % requires is my own lib loader. ignore here.

% predicates used by spuds:

% blocked_sys_library( 'jpl.pl' ).

doc_server_default( port, 4002 ).
% doc_server_default( start_opens_browser, true).

% file_is_blocked_prolog_source( +File ).

% file_is_prolog_source( +File ).

% prolog_source_directory( -Dir ).
prolog_source_directory( Dir ) :-
	expand_file_name( '$HOME/pl/lib/src', [Dir] ).
% prolog_source_directory( -Dir, -Opts ).

% prolog_source_file( -File ).

% spuds_debug( false ).
