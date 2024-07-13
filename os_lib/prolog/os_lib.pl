:- module( os_lib, [ 
              % name manipulators
              os_base/2,                 % +Os, -Bname
              os_ext/2,                  % ?Ext, +Os
              os_ext/3,                  % ?Ext, ?Stem, ?Os
              os_ext/4,                  % ?Ext, ?NewExt, +Os, -NewOs
              os_dir_stem_ext/2,         % -Os, +Opts
              os_dir_stem_ext/4,         % ?Dir, ?Stem, ?Ext, ?Os
              os_stem/3,                 % ?Stem, -Os, +Opts
              os_postfix/2,              % -PsfxS, +Posted
              os_postfix/3,              % +PsfxS, ?Fname, ?Posted
              os_postfix/4,              % +PsfxS, ?Fname, ?Posted, +Opts
              os_abs/2, os_abs/3,        % +Os, -Abs[, +Opts]
              os_path/2,                 % ?Parts, ?Path
              os_path/3,                 % +-Dir, +-File, -+Path
              os_slashify/2,             % +-Path, -+Slashed
              os_parts/2,                % +-Parts, -+Stem
              os_parts/3,                % +-Parts, -+Stem, +Opts
              os_unique/2,               % +Tkn, -Os
              os_unique/3,               % +Tkn, -Os, +Opts

              % commands
              os_mv/2,                   % +From, +To
              os_cp/2,                   % +From, +To
              os_ln_s/2,                 % +From, +To
              os_rm/1, os_rm/2,          % see os_remove/1,2
              os_remove/1,               % +Os
              os_remove/2,               % +Os, +Opts
              os_make_path/1,            % +Os
              os_make_path/2,            % +Os, +Opts
              os_repoint/2,              % +Os, +Opts 
              os_mill/4,                 % +Os, +Goal, ?Milled, +Opts
              os_un_zip/3,               % +Os, ?Stem, +Opts

              % helpers
              os_sep/1,                  % -Sep
              os_sep/2,                  % -Sep, +Opts
              os_sel/3,                  % +Oses, +Pattern, -Selected
              os_sel/4,                  % +Oses, +Pattern, -Selected, +Opts
              os_term/2,                 % +-Atom, -+SlashTerm
              os_name/2,                 % +Os, -Type

              % types and casting
              % os_type_entity/3,         % +Os, +Type, -Typed
              os_cast/2,                 % +Os, -Typed
              os_cast/3,                 % +Type, +Os, -Typed
              os_tmp_dir/1,              % -Os
              os_type_base/2,            % ?Type, ?Base
              os_version/2,              % -Vers, -Date

              % logical
              os_exists/1, os_exists/2,  % +Os[, +Opts]
              os_file/1, os_file/2,      % ?Os[, +Opts]
              os_files/1, os_files/2,    % -Os[, +Opts]
              os_dir/1, os_dir/2,        % ?Os[, +Opts]
              os_dirs/1, os_dirs/2,      % ?Os[, +Opts]

              % operators
              op( 400, fx, / )
            ] ).

/**  <module> Operating system interaction predicates.

This library collects a number of predicates useful for OS interactions. 
The emphasis here is on operations on files and directories rather than on
calling OS commands. Unlike the system predicates of SWI/Yap here we adhere
to the <lib>_ convention prefix that allows for more succinct predicate names.
The assumption is that by using prefix "os", there will be a main argument
that is an OS entity, so the predicate name does not have to explicitly refer to 
all the arguments. For instance 

---++ Highlights
  * polymorphic 
    4 ways to name OS objects
  * casting
    and particulalry output variable casting via variable decoration
  * os_exists( Os, Opts )
    works on files and dirs by default, and can be specialised via Opts
  * os_postfix( Psfx, Os, Posted )
    os_postfix/3 add a bit on a OS name to produce a new one
  * os_ext( Ext, Stem, Os )
    os_ext/3 is a renamed file_name_extension/3 with few extra bits
  * os_unique( Token, Os, Opts )
    constructs unique filenames either based ondate (and possible time stamp) or on versioning
  * os_dir_stem_ext( Dir, Stem, Ext, Os )
    os_dir_stem_ext/4 construct and de-construct OS names from/to its main parts
  * os_dir_stem_ext( Os, Opts )
    construct 
  * os_mill(File,Goal,Milled,Opts)
    os_mill/4 allows construction of evolving pipelines
  * os_file(File)
    os_file/1 backtrack over all files in current directory

In addition, the library is polymorphic in naming OS objects
by supporting 4 different os term structures: 
  * /-terms, 
  * atoms,
  * strings, and
  * aliased terms.

Currently the emphasis is on file name manipulations but command 
(eg copy_file) are likely to be included in new versions. Main reason why 
they are not yes, is that we use pack(by_unix).

---++ Installation

To install
==
?- pack_install( os_lib ).
==

to load
==
?- use_module( library(os) ).
==

or 
==
?- use_module( library(os_lib) ).
==

---+++ Opts 

The library attempts to keep a consistent set of options that are on occasions 
funnelled through to either other interface or commonly used private predicates.

Common options:

  * dir(Dir='.')
    directory for input and output

  * idir(Idir='.')
    directory for input (overrides Dir)

  * odir(Odir='.')
    directory for output (overrides Dir)

  * ext(Ext='')
    extension for file

   * stem(Stem)
     stem to be used for constructing os names

   * sub(Sub)
     apply operation recursive to sub directories

Variable name conventions
  * var(Os)
    An os entity 

---+++ Casts 
(os_cast/3,os_cast/2)
  * \(Os)
    casts to /-terms
  * +(Os) 
    casts to atoms
  * &(Os)
    casts to strings
  * '@'(Os) 
    casts to alias (input must be an alias to start with)

---+++ Nonmeclature
  * /-(term)
    Pronounced slash-term. Is an Os entity expressed as a term starting or separated by / for example abc/foo.bar instead of 'abc/foo.bar'
  * atomic(+os)
    is an atom referring to an OS object
  * string(&os)
    is a stringreferring to an OS object
  * alias(@term)
    is an Os entity expressed with alias compound, for example 
    abc(data/foo.bar) where abc is a known path alias

  * (16.6.24) i think the common adjective fo dir, link and fiel should be os-name

---++ Predicates
The library predicates can be split to 4 groups.

1. Predicates for manipulating and constructing OS entity names
  * os_abs/2,3
  * os_base/2
  * os_ext/3
  * os_dir_stem_ext/4
  * os_dir_stem_ext/2
  * os_postfix/3
  * os_path/2
  * os_path/3
  * os_parts/3
  * os_slashify/2
  * os_stem/3
  * os_unique/3

2. Commands
  * os_make_path/2
  * os_mill/4
  * os_mv/2,
  * os_cp/2,
  * os_remove/2 (os_rm/2)
  * os_repoint/2
  * os_un_zip/3

3. Logical
  * os_dir/1,os_dir/2
  * os_dirs/1,os_dirs/2
  * os_exists/2
  * os_file/1,os_file/2
  * os_files/1,os_files/2

4. Helpers
  * os_cast/3
  * os_sep/2
  * os_sel/4
  * os_separator/2
  * os_term/2
  * os_name/2
  * os_tmp_dir/1
  * os_version/2

---++ Info
@author   nicos angelopoulos
@version  0.0.1 2015/4/30
@version  0.0.2 2015/4/30  added module documentation
@version  0.0.3 2015/12/10 redone the typing and added better alias support, started custom errors
@version  0.1.0 2016/2/24  first publisc release
@version  0.6.0 2017/3/10  works with pack(lib)
@version  1.0.0 2018/3/18  
@version  1.2.0 2018/8/5   added os_files/1,2 and os_dirs/1,2 (with options) and removed os_dir_files/2 and os_dir_dirs/2.  
@version 1.3 2018/10/1 cleaner error handling via throw, new opt dots(D), os_cast/3 arguments switch
@version 1.4 2019/4/22 option sub(Sub); cp_rec.pl script; list of postfixes, etc
@version 1.5 2019/4/22 os_path/2, fixes and new options to os_mill/4 ; os_exists/2 (return type) & os_sel/4
@version 1.6 2022/6/14 fixed: os_exist( file, type(link) ), added option read_link(RLnk) to os_file/2
@version 1.7 2024/2/7  fixes in: os_unique/3 and os_ext/4
@see http://www.stoics.org.uk/~nicos/sware/os
@see http://www.stoics.org.uk/~nicos/sware/os/html/os_lib.html
@see doc/Releases.txt
@tbd os_pwd/1  (working_directory + casting)
@tbd use os_path/3 as a template to convert all lib predicates to castable outputs.
@tbd there might a bit of dead code around

*/

:- use_module(library(lists)).      % select/3,...
:- use_module(library(apply)).      % maplist/3,...
:- use_module(library(date)).       % date_time_value/3.
:- use_module(library(debug)).      % /1,3.  -> switch to debuc/1,3
:- use_module(library(filesex)).    % link_file/3, make_directory_path/1.
:- use_module(library(prolog_source)).    % file_alias_path/2.

:- use_module(library(lib)).
:- lib(source(os_lib), homonyms(true)).

:- lib(options).
:- lib(pack_errors).

:- lib(os_dir_stem_ext/2).
:- lib(os_dir_stem_ext/4).
:- lib(os_stem/3).
:- lib(os_ext/2).
:- lib(os_ext/3).
:- lib(os_ext/4).
:- lib(os_rm/2).
:- lib(os_rm/3).
:- lib(os_remove/3).
:- lib(os_remove/2).
:- lib(os_make_path/1).
:- lib(os_make_path/2).
:- lib(os_mill/4).
:- lib(os_un_zip/3).
:- lib(os_parts/2).
:- lib(os_parts/3).
:- lib(os_path/2).
:- lib(os_path/3).
:- lib(os_postfix/2).
:- lib(os_postfix/3).
:- lib(os_postfix/4).
:- lib(os_repoint/2).
:- lib(os_slashify/2).
:- lib(os_term/2).
:- lib(os_tmp_dir/1).
:- lib(os_name/2).
:- lib(os_unique/2).
:- lib(os_unique/3).
:- lib(os_base/2).
:- lib(os_cast/2).
:- lib(os_cast/3).
:- lib(os_errors/0).
:- lib(os_abs/2).
:- lib(os_abs/3).
:- lib(os_file/1).
:- lib(os_file/2).
:- lib(os_files/1).
:- lib(os_files/2).
:- lib(os_dir/1).
:- lib(os_dir/2).
:- lib(os_dirs/1).
:- lib(os_dirs/2).
:- lib(os_exists/1).
:- lib(os_exists/2).
:- lib(os_sep/1).
:- lib(os_sep/2).
:- lib(os_sel/3).
:- lib(os_sel/4).
:- lib(os_mv/2).
:- lib(os_cp/2).
:- lib(os_ln_s/2).
:- lib(os_type_base/2).

:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:holds/2).
:- lib(stoics_lib:compound/3).

:- lib(end(os_lib)).

/** os_version( -Version, -Date ).

Current version and release date for the pack.

==
?- os_version( V, D ).
V = 1:7:0,
D = date(2024,1,7)
==

*/
os_version( 1:7:0, date(2024,2,7) ).
