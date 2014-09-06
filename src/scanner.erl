-module(scanner).
-include_lib("kernel/include/file.hrl").
-compile(export_all).

start(Entry) ->
	spawn(?MODULE, collector, [[]]),
	discover(Entry).

collector() ->
	register(collector, self()),
	receive
		{Entry, Hash} ->
			[Hash|List],
			io:format("~p~n", [Hash])
	end,
	unregister(collector),
	collector().

discover(Entry) ->
	{ok, EntryRead} = file:read_file_info(Entry),
	case EntryRead#file_info.type of
		directory ->
			exploredir(Entry);
		regular ->
			spawn(?MODULE, fileinfo, [Entry])
	end.

exploredir(Directory) ->
	{ok, Filenames} = file:list_dir(Directory),
	[discover(Directory ++ "/" ++ X) || X <- Filenames].

fileinfo(Entry) ->
	
	collector ! {Entry, os:cmd("md5hash " ++ Entry)}.