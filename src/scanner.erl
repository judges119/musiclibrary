-module(scanner).
-include_lib("kernel/include/file.hrl").
-export([start/2, discover/1, file_info/1]).

start(Entry, song_db) ->
	spawn(?MODULE, discover, [Entry]).

discover(Entry) ->
	{ok, EntryRead} = file:read_file_info(Entry),
	case EntryRead#file_info.type of
		directory ->
			explore_dir(Entry);
		regular ->
			case file_info(Entry) of
				{Location, {Song, Artist, Album, Genre, Year, Composer, {TrackNumber, TrackCount}, {SetNumber, SetCount}}, MD5Hash} ->
					%[_, {rows, [{Count}]}] = sqlite3:sql_exec(song_db, "SELECT count(*) FROM song WHERE location = '" ++ Location ++ "'"),
					%if Count >= 1 ->
							
					sqlite3:write(song_db, song, [{title, Song}, {artist, Artist}, {album, Album}, {genre, Genre}, {md5hash, MD5Hash}, {location, Location}]);
				_ ->
					ok
			end
	end.

explore_dir(Directory) ->
	{ok, Filenames} = file:list_dir(Directory),
	[spawn(?MODULE, discover, [Directory ++ "/" ++ X]) || X <- Filenames].

file_info(Entry) ->
	case (string:str(Entry, ".DS_Store") == 0) and (string:str(Entry, "thumbs.db") == 0) of
		true ->
			{Entry, get_tags(Entry), os:cmd("md5hash " ++ Entry)};
		false ->
			ok
	end.

%%
%% Gets ID3v2 tag information from a song
%%
%% Currently a lazy implementation, I should abstract the majority out for the DRY principle but that's for another day
%%
get_tags(Entry) ->
	Song = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TT2 (Title/songname/content description)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | tr -d '\n'" ),
	Artist = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TP1 (Lead performer(s)/Soloist(s))' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | tr -d '\n'"),
	Album = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TAL (Album/Movie/Show title)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | tr -d '\n'"),
	Genre = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TCO (Content type)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | tr -d '\n'"),
	Year = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TYE (Year)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | tr -d '\n'"),
	Composer = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TCM (Composer)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | tr -d '\n'"),
	TrackNumber = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TRK (Track number/Position in set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 1 | tr -d '\n'"),
	TrackCount = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TRK (Track number/Position in set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 2 | tr -d '\n'"),
	SetNumber = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TPA (Part of a set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 1 | tr -d '\n'"),
	SetCount = os:cmd("id3v2 -l '" ++ Entry ++ "' | grep 'TPA (Part of a set)' | cut -d ':' -f 2- | sed -e 's/^[ \t]*//' | cut -d '/' -f 2 | tr -d '\n'"),
	{Song, Artist, Album, Genre, Year, Composer, {TrackNumber, TrackCount}, {SetNumber, SetCount}}.