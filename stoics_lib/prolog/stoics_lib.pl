:- module( stoics_lib, [ 
              stoics_lib/0,            % doc predicate 
              stoics_lib_version/2,    % -Vers, -Date
            % atom/
              at_con/2, at_con/3,      % ?List[, +Sep], ?Atom
              atom_replace/4,          % +Atom, +What, +With, -New
              atom_sub/2,              %  ?Sub, ?Ful.
              prefix_atom/2,prefix_atom/3,  % +Prefix, +Atom[, -Postfix]
              sub_atom/2,sub_atom/4,   % +Full[, ?Pre, ?Post], Sub
            % codes
              codes_n_digits/3,        % -InCodes, +N, -Codes
              n_digits_integer_codes/3,%( +N, +Numb, -Codes ).
            % date_time/
              datime_readable/1,       % +Ratom
              datime_readable/2,       % +Datime, -Ratom
              date_two_digit_dotted/1, % -Dotted
              date_two_digit_dotted/2, % +Date, -Dotted
              get_date/1,              % -Date
              get_date_time/1,         % -CurrDateTime
              get_datetime/1,          % -CurrDateTime
              three_letter_month/2,    % ?IntIdx, -Month
              three_letter_months/1,   % -Months
            % error/
              message_report/3,        % +Format, +Args, +Kind
            % file/
              expand_spec/2,           % +FileSpec, -Expanded
              locate/3,                % +File, +Exts, -Locations
            % io/
              io_line/2,               % +Stream, ?Line
              io_get_line/2,           % +Stream, -Line
              io_put_line/2,           % +Stream, +Line
              io_lines/2,              % +FileOrStream, -Lines
              io_close/2,              % +FileR, -Stream
              io_open/3,               % +FileR, +Mode, -Stream
              io_sections/3,           % +File, -Sections, +Opts
              io_streams/3,            % ?Input, ?Output, ?Error
            % kv/
              kv_compose/3,            % +Ks, +Vs, -KVs
              kv_compose/4,            % +List1, +List2, -KVsCont, -Tkvs
              kv_decompose/3,          % +Pairs, -Ks, -Vs
              kv_ks/2,                 % +KVs, -Ks
              kv_vs/2,                 % +KVs, -Vs
              kvo_k_memberchk/3,       % +K, +KVs, -V
              kvs_k_memberchk/3,       % +K, +KVs, -V
            % list/
              break_on_list/4,         % +List, +Partial, -Left, -Right
              break_nth/4,             % +Nth, +List, -Left, -Right
              n_breaks/4,              % +Vector, +N, -Breaks, -Opts
              nth1/5,                  % ?N, +List, ?With, ?Nth, +NewList
              has_at_least/3,          % +N, +X, +List
              has_at_most/3,           % +N, +X, +List
              list_frequency/2,        % +List, -Freqs
              list_frequency/3,        % +List, -Freqs, +Opts
              list_proportions/2,      % +List, -Props
              list_proportions/3,      % +List, -Props, +Opts
              list_transpose/2,        % +List, -Transpose
              min_max/3,               % +List, -Max, -Min
              select_all/4,            % +List, +Elem, -Select, -Rem
              select_first/3,          % +List, +Elem, -Rem
              skim/3,                  % +Nested, -Scum, -Remains
            % meta/
              call_morph/4,            % +Term, +Input, -Morphed, +Opts
              current_call/1,          % +Goal
              current_call/2,          % +Goal, +Else
              goal/4,                  % +Partial, +ArgS, +Mod, -Goal
              goal_spec/2,             % +ModG, -ModSpec
              holds/2,                 %  +Goal, -Holds
              imported_from/2,         %  +Clauser, ?Mod
              known/1,known/2,         %
              known/3,                 % +Goal[, +Tkn], +Cat]
              maparg/2,maparg/3,       % +Pname, ?Term1[, ?Term2]
              maparg/4,                % +PName, +Npos ?Term1, ?Term2
              map_list_options/3,      % +Goal, +InList, +Opts
              map_list_options/4,      % +Goal, +/-InList, -/+OutList, +Opts
              map_succ_list/3,         %  
              map_succ_list/4,         % +Goal, ?InList, ?OutList[, -Rejects]
              mod_goal/2,              % +Goal, -Moal
              mod_goal/3, mod_goal/4,  % +Mod, +Goal[, +Override], -Moal
              on_fail/2, on_fail/3,    % +Goal, +Call, +Opts
              on_call/4,               % +OnB+ +Goal, +In, -Out
              which/3,                 % Goal, +Term, -Indices
            % number/
              int_trailer/2,           % +Int, -Trailer
              compare_arithmetic/3,    % -Op, +X, +Y
              n_digits_min/3,          % +N, +Number, -Padded 
            % strings/
              letter_strings/3,        % +Start, -N, -Letts 
            % term/
              arg_add/4,               % +N, +Term, +Arg, -New
              arg/4, arg/5,            % ?N, +TermIn, [+NewNth,] ?Nth, -TermOut
              arity/2,arity/3,         % ?Term[, ?Name], ?Arity
              functor_term/2,          % ?Pid, ?Term
              compare/4,               % +Type, ?Op, +Term1, +Term
              compound/3,              % +Term, -Name, -Args 
              curtail/3,               % +Term, -Max, -Curtailed
              en_append/3,             % +ListOr1, +ListOr2, List
              en_list/2,en_list/3,     % +Term, -Listed[, +Opts]
              has_length/2,            % +List, +Lengthy
              has_length/3,            % +List, +Lengthy, +Op
              has_length/4,            % +List, +Lengthy, +Op, +Err
              op_compare/3,            % ?Op, +Term1, +Term2
              portray_clauses/2,       % +Terms, +Opts
              positions/2,positions/3, % +Data, [-Dtype,] -NofPositions ).
              position/3,position/4,   % [+Type,] ?N, +Data, ?Nths
              position/6,              % +Type, ?N, +Data, ?Nths, -NxN, -Cont
              position_nth/3, position_nth/4, % +N, +Data[, -Nth]
              position_nth/5, position_nth/6, % [+Dtype,] +N, +Data, -Nth, -Rem, -Nxt
              position_type/2,         % +Term, -PosType
              term_length/2,           % +Term, -Length
              term_type/2,             % +Term, -Type
              termplate/2,termplate/3, % +Term[, -Arity], -Termplate
              url_file/2,url_file/3    % +Url, +File[, +Opts]
    ] ).

% :- ensure_loaded( '../src/auxil/stoics_lib_module.pl' ).

:- use_module(library(lib)).
:- lib( source(stoics_lib), [index(true),homonyms(false)] ).

:- lib(stoics_lib/0).
:- lib(stoics_lib_version/2).
:- lib(at_con/2).
:- lib(atom_sub/2).
:- lib(prefix_atom/2).
:- lib(sub_atom/2).
:- lib(codes_n_digits/3).
:- lib(n_digits_integer_codes/3).
:- lib(datime_readable/1).
:- lib(datime_readable/2).
:- lib(date_two_digit_dotted/1).
:- lib(date_two_digit_dotted/2).
:- lib(get_date/1).
:- lib(get_date_time/1).
:- lib(get_datetime/1).
:- lib(three_letter_month/2).
:- lib(three_letter_months/1).
:- lib(message_report/3).
:- lib(expand_spec/2).
:- lib(io_line/2).
:- lib(io_get_line/2).
:- lib(io_put_line/2).
:- lib(io_lines/2).
:- lib(io_close/2).
:- lib(io_open/3).
:- lib(kv_compose/3).
:- lib(kv_compose/4).
:- lib(kv_decompose/3).
:- lib(kv_ks/2).
:- lib(kv_vs/2).
:- lib(kvo_k_memberchk/3).
:- lib(kvs_k_memberchk/3).
:- lib(break_on_list/4).
:- lib(break_nth/4).
:- lib(has_at_least/3).
:- lib(has_at_most/3).
:- lib(has_length/2).
:- lib(has_length/3).
:- lib(has_length/4).
:- lib(list_frequency/2).
:- lib(list_frequency/3).
:- lib(list_proportions/2).
:- lib(list_proportions/3).
:- lib(list_transpose/2).
:- lib(select_all/4).
:- lib(select_first/3).
:- lib(skim/3).
:- lib(current_call/1).
:- lib(current_call/2).
:- lib(goal/4).
:- lib(goal_spec/2).
:- lib(holds/2).
:- lib(imported_from/2).
:- lib(known/1).
:- lib(known/2).
:- lib(known/3).
:- lib(map_list_options/4).
:- lib(map_succ_list/4).
:- lib(mod_goal/2).
:- lib(mod_goal/3).
:- lib(mod_goal/4).
:- lib(which/3).
:- lib(int_trailer/2).
:- lib(letter_strings/3).
:- lib(arity/2).
:- lib(arity/3).
:- lib(functor_term/2).
:- lib(compound/3).
:- lib(en_list/2).
:- lib(op_compare/3).
:- lib(portray_clauses/2).
:- lib(positions/2).
:- lib(positions/3).
:- lib(position/3).
:- lib(position/4).
:- lib(position/6).
:- lib(position_nth/3).
:- lib(position_nth/4).
:- lib(position_nth/5).
:- lib(position_nth/6).
:- lib(position_type/2).
:- lib(termplate/2).
:- lib(termplate/3).
:- lib(locate/3).
:- lib(compare/4).
:- lib(compare_arithmetic/3).
:- lib(n_digits_min/3).
:- lib(n_breaks/4).
:- lib(min_max/3).
:- lib(nth1/5).
:- lib(arg_add/4).
:- lib(arg/4).
:- lib(arg/5).
:- lib(maparg/2).
:- lib(maparg/3).
:- lib(maparg/4).
:- lib(atom_replace/4).
:- lib(io_sections/3).
:- lib(on_fail/2).
:- lib(on_fail/3).
:- lib(on_call/4).
:- lib(term_length/2).
:- lib(curtail/3).
:- lib(term_type/2).
:- lib(en_append/3).
:- lib(url_file/2).
:- lib(call_morph/4).

:- lib( end(stoics_lib) ).
