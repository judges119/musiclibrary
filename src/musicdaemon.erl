-module(musicdaemon).
-export([start/2, periodic_update_scan/2, periodic_removal_scan/1, manual_removal_scan/1]).

start(LibraryPaths, DBPath) ->
	sqlite3:open(song_db, [{file, DBPath}]),
	TableInfo = [{id, integer, [{primary_key, [asc, autoincrement]}]}, {title, text}, {artist, text}, {album, text}, {genre, text}, {md5hash, text}, {location, text}],
	sqlite3:create_table(song_db, song, TableInfo),
	[spawn(scanner, start, [X, song_db]) || X <- LibraryPaths],
	[spawn(?MODULE, periodic_update_scan, [X, song_db]) || X <- LibraryPaths],
	spawn(scanner, removal, [song_db]),
	spawn(?MODULE, periodic_removal_scan, [song_db]).

periodic_update_scan(LibraryPath, DB) ->
	timer:apply_interval(43200000, scanner, start, [LibraryPath, DB]).

periodic_removal_scan(DB) ->
	timer:apply_interval(43200000, scanner, removal, [DB]).

manual_update_scan(LibraryPath, DBPath) ->
	sqlite3:open(song_db, [{file, DBPath}]),
	scanner:start(LibraryPath, song_db).

manual_removal_scan(DBPath) ->
	sqlite3:open(song_db, [{file, DBPath}]),
	scanner:removal(song_db).