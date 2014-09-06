-module(musicdaemon).
-export([start/2, periodic_scan/2]).

start(LibraryPaths, DBPath) ->
	sqlite3:open(song_db, [{file, DBPath}]),
	TableInfo = [{id, integer, [{primary_key, [asc, autoincrement]}]}, {title, text}, {artist, text}, {album, text}, {genre, text}, {md5hash, text}, {location, text}],
	sqlite3:create_table(song_db, song, TableInfo),
	[spawn(scanner, start, [X, song_db]) || X <- LibraryPaths],
	[spawn(?MODULE, periodic_scan, [X, song_db]) || X <- LibraryPaths].

periodic_scan(LibraryPath, DB) ->
	timer:apply_interval(43200000, scanner, start, [LibraryPath, DB]).

manual_scan(LibraryPath, DB) ->
	scanner:start(LibraryPath, DB).

