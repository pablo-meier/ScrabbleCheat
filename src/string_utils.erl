-module(string_utils).
-import(re, [replace/4]).
-export([format_string_for_trie/1]).


%% format_string_for_trie :: String -> String
%% 
%% Tests the line for a newline, strips it and all whitespace.  
%% Converts to uppercase.
format_string_for_trie(Line) ->
	No_Newline = replace(Line, "\\s+", "", [global, {return, list}]),
	string:to_upper(No_Newline).
