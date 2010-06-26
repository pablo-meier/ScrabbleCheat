-module(dict_parser).
-export([parse/1]).
-import(tries, [empty_trie/0, add_word/2]).

parse(Filename) ->
	Result = file:open(Filename, read),
	case Result of
		{ok, FileHandle} ->
			Empty = empty_trie(),
			parse_each_line(FileHandle, Empty);
		{error, Reason} ->
			io:format("Whoops! Can't read file -> ~p~n", [Reason])
	end.

%% parse_each_line
parse_each_line(IoHandle, Curr) ->
	Line = io:get_line(IoHandle, ''),
	case Line of
		eof ->
			Curr;
		A_Line ->
			Stripped = string_utils:format_string_for_trie(A_Line),
			io:format("Parsing ~p~n", [Stripped]),
			NewTrie = add_word(Stripped, Curr),
			parse_each_line(IoHandle, NewTrie)
	end.

