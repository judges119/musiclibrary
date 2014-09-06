-module(scanner).
-include_lib("kernel/include/file.hrl").
-export([start/1, collector/1, fileinfo/1]).

start(Entry) ->
	sqlite3:open(song_db, [in_memory]),
	TableInfo = [{id, integer, [{primary_key, [asc, autoincrement]}]}, {title, text}, {artist, text}, {album, text}, {genre, text}, {md5hash, text}],
	ok = sqlite3:create_table(song_db, song, TableInfo),
	spawn(?MODULE, collector, [song_db]),
	discover(Entry).

collector(song_db) ->
	register(collector, self()),
	receive
		{_, {Song, Artist, Album, Genre, Year, Composer, {TrackNumber, TrackCount}, {SetNumber, SetCount}}, MD5Hash} ->
			sqlite3:write(song_db, song, [{title, Song}, {artist, Artist}, {album, Album}, {genre, Genre}, {md5hash, MD5Hash}])
	end,
	io:format("SQLite: ~p~n", [sqlite3:read_all(song_db, song, [title])]),
	unregister(collector),
	collector(song_db).

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
	case (string:str(Entry, ".DS_Store") == 0) and (string:str(Entry, "thumbs.db") == 0) of
		true ->
			collector ! {Entry, get_tags(Entry), os:cmd("md5hash " ++ Entry)};
		false ->
			ok
	end.

%%
%% Gets ID3v2 tag information from a song
%%
%% Currently a lazy implementation, I should abstract the majority out for the DRY principle but that's for another day
%%
get_tags(Entry) ->
	Song = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TT2 (Title/songname/content description)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//'"),
	Artist = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TP1 (Lead performer(s)/Soloist(s))' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//'"),
	Album = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TAL (Album/Movie/Show title)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//'"),
	Genre = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TCO (Content type)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//'"),
	Year = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TYE (Year)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//'"),
	Composer = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TCM (Composer)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//'"),
	TrackNumber = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TRK (Track number/Position in set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 1"),
	TrackCount = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TRK (Track number/Position in set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 2"),
	SetNumber = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TPA (Part of a set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 1"),
	SetCount = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TPA (Part of a set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 2"),
	{Song, Artist, Album, Genre, Year, Composer, {TrackNumber, TrackCount}, {SetNumber, SetCount}}.