-module(dict_parser).
-export([parse/1]).
-import(tries, [empty_trie/0, add_word/2]).
-import(re, [compile/1,replace/4]).

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
			Stripped = format_for_dict(A_Line),
			io:format("Parsing ~p~n", [A_Line]),
			NewTrie = add_word(Stripped, Curr),
			parse_each_line(IoHandle, NewTrie)
	end.


%% format_for_dict :: String -> String
%% 
%% Tests the line for a newline, strips it and all whitespace.  
%% Converts to uppercase.
format_for_dict(Line) ->
	No_Newline = replace(Line, "\\s+", "", [global]),
	string:to_upper(No_Newline).
