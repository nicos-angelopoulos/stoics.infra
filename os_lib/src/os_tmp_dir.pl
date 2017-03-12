%% os_tmp_dir( -Tmp ).
%
%  Creates a uniquely named directory.
%
%  Contrary to system tmp_file_stream/3 %  this predicate does not remove the
%  directory at halt. The directory is placed in /tmp/ so it wouldn't surive a reboot.
%
% @author nicos angelopoulos
% @version  0.1 2014/4/2  (some time well before)
%
os_tmp_dir( Dir ) :-
	tmp_file_stream( text, Dir, Stream ),
	close( Stream ),
	delete_file( Dir ),
	make_directory( Dir ).
