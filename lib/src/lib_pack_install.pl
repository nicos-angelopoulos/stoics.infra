/** lib_pack_install( +Pack ).

Wrapper for pack_install. Mostly to deal with lib binaries incompatibility
introduced in SWI 8.2.0.

@author nicos angelopoulos
@version  0.1 2020/05/29
*/
lib_pack_install( Pack ) :-
    lib_pack_install_known_issue( Pack, Loc ),
    !,
    Mess1 = 'There is a known compatibility issue with you SWI version and pack:~w.',
    lib_message_report( Mess1, [Pack], informational ),
    Mess2 = 'Installing from: ~w',
    lib_message_report( Mess2, [Loc], informational ),
    pack_install( Loc ).

lib_pack_install_known_issue( real, Loc ) :-
    current_prolog_flag( version, Vers ), 
    80200 < Vers,
    Loc = 'http://stoics.org.uk/~nicos/sware/packs/real/real-2.0.tgz'.
lib_pack_install_known_issue( prosqlite, Loc ) :-
    current_prolog_flag( version, Vers ), 
    80200 < Vers,
    Loc = 'http://stoics.org.uk/~nicos/sware/packs/prosqlite/prosqlite-1.5.tgz'.
