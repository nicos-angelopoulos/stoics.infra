
:- use_module( library(prolog_pack) ).
% from which this has been curved out

:- multifile prolog:message//1.

prolog:message(pack(Message)) -->
	message(Message).

message(confirm(Question,Default)) -->
	message(Question),
	prolog_pack:confirm_default(Default),
	[ flush ].

message(contact_server(Pack)) -->
    ['Library: ~w, not locally installed. Do you want me to search on the SWI server for matching packs ?'-[Pack] ].
message(pack_on_server(suggests,Pack)) -->
	[ 'Suggested lib not found locally, Above are possible replacements on the server.\nDo you want to run ?- pack_install(~w).'-[Pack] ].
message(pack_on_server(self,Pack)) -->
	[ 'Lib not found locally, Above are possible replacements on the server.\nDo you want to run ?- pack_install(~w).'-[Pack] ].
% message(expected_not_defined(ExpPid,Cxt)) -->
	% [ ''-[Cxt,ExpPid] ].
