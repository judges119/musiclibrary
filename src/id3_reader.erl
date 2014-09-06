-module(id3_reader).
-compile(export_all).

read_file(FileToRead) ->
	{ok, Fd} = file:open(FileToRead, [binary]),
	case file:pread(Fd, 0, 3) of
		{ok, << "ID3" >>} ->
			Version = check_version(Fd),
			Unsync = check_flag(Fd, 0),
			Extended = check_flag(Fd, 1),
			Experimental = check_flag(Fd, 2),
			io:format("~p~n", [Experimental]),
			Size = check_size(Fd),
			io:format("~p~n", [Size]),
			if Extended ->
					{ok, CRCFlag} = file:pread(Fd, 14, 2),
					if CRCFlag bsr 15 == 1 ->
							find_tags(Fd, Unsync, Size - 10, 20, []);
						CRCFlag bsr 15 /= 1 ->
							find_tags(Fd, Unsync, Size - 6, 16, [])
					end;
				not Extended ->
					find_tags(Fd, Unsync, Size, 10, [])
			end;
		{ok, Temp} ->
			io:format("~p~n", [Temp]),
			{error, "Failed to find ID3 tags"}
	end.

check_version(Fd) ->
	case file:pread(Fd, 3, 2) of
		{ok, << 2, _ >>} ->
			"ID3v2.3";
		{ok, Temp} ->
			io:format("~p~n", [Temp]),
			{error, "Incorrect version of ID3 tags"}
	end.

check_flag(Fd, Shift) ->
	case file:pread(Fd, 5, 1) of
		{ok, << Temp >>} ->
			((Temp bsl Shift) bsr 7) == true;
		_ ->
			false
	end.

check_size(Fd) ->
	{ok, << First:8, Second:8, Third:8, Fourth:8 >>} = file:pread(Fd, 6, 4),
	(First bsl 21) bor (Second bsl 14) bor (Third bsl 7) bor Fourth.

find_tags(_Fd, _Unsync, 0, _Start, Tags) ->
	Tags;
find_tags(Fd, Unsync, Size, Start, Tags) ->
	case file:pread(Fd, Start, 4) of
		{ok, << "TALB" >>} ->
			%%{ok, Title} = file:pread(Fd, Start + 10 + 1, FrameSize),
			%%io:format("~p~n", [Title]),
			%%find_tags(Fd, Unsync, Size - 10 - FrameSize, Start + 10 + FrameSize, [Title | Tags]);
			{ok, "Yay!"};
		{ok, ID} ->
			io:format("ID: ~p~n", [binary:bin_to_list(ID)]),
			{ok, << FrameSize:32 >>} = file:pread(Fd, Start + 4, 4),
			file:pread(Fd, Start + 8, 2),
			io:format("FrameSize: ~p~n", [FrameSize]),
			find_tags(Fd, Unsync, Size - 10 - FrameSize, Start + 10 + FrameSize, Tags)
	end.