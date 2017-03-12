os_errors.

:- multifile( pack_errors:message/3 ).

pack_errors:message( nested_alias(Pos,Pid,Os) ) -->
	['Nested aliases are not supported yet.\nFound: ~w at position: ~d for predicate: ~w'-[Os,Pos,Pid]].

pack_errors:message( missing_alias(Os) ) -->
	['OS entity: ~w, looks like aliased but alias does not exist.'-[Os]].

pack_errors:message( os_postfix_lists(Sc,Fr) ) -->
	['OS entity: ~w (2nd arg) should not be a list when options (4th arg) is: ~w'-[Sc,Fr]].

pack_errors:message( os_exists(Os) ) -->
	['OS entity: ~w, already exists'-[Os]].

pack_errors:message( os_exists_not(Os) ) -->
	['OS entity: ~w, does not exist'-[Os]].

pack_errors:message( os_mode(Os,Mode) ) -->
	['OS entity: ~w, exists, but it not of mode: ~w'-[Os,Mode]].

pack_errors:message( os_mode_undefined(Os,Type,Mode) ) -->
	['OS entity: ~w, of type: ~w, cannot be tested for mode: ~w'-[Os,Type,Mode]].

pack_errors:message( os_type(Os,Needs,Has) ) -->
	['OS entity: ~w, not of requested type: ~w, but has type: ~w'-[Os,Needs,Has]].

pack_errors:message( os_created_not(Milled,From) ) -->
	['OS milled: ~p  was not created (source was: ~p)'-[Milled,From]].

pack_errors:message( os_unkown_type(Os) ) -->
	['OS entity: ~w, is of unknown type'-[Os]].

pack_errors:message( os_plate_cast(Term,Def,Tplate) ) -->
	['OS entity: ~w, can not be cast to term-plate: ~w, with default: ~w'-[Term,Def,Tplate]].
