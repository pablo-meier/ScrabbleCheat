-module(string_utils).
-import(re, [replace/4]).
-export([format_string_for_gaddag/1]).


%% format_string_for_gaddag :: String -> String
%% 
%% Tests the line for a newline, strips it and all whitespace.  
%% Converts to uppercase.
format_string_for_gaddag(Line) ->
    No_Newline = replace(Line, "\\s+", "", [global, {return, list}]),
    string:to_upper(No_Newline).
