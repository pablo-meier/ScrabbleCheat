%% Copyright (c) 2010 Paul Meier
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(dict_parser).
-export([parse/1]).
-import(gaddag, [empty_gaddag/0, add_word/2]).

parse(Filename) ->
	case file:open(Filename, read) of
		{ok, FileHandle} ->
			Empty = empty_gaddag(),
			io:format("Parsing...~n"),
			parse_each_line(FileHandle, Empty);
		{error, enoent} ->
			throw({file_not_found, Filename})
	end.

%% parse_each_line
parse_each_line(IoHandle, Curr) ->
	Line = io:get_line(IoHandle, ''),
	case Line of
		eof ->
			io:format("Done Parsing.~n"),
			Curr;
		A_Line ->
			Stripped = string_utils:format_string_for_gaddag(A_Line),
			NewGaddag = add_word(Stripped, Curr),
			parse_each_line(IoHandle, NewGaddag)
	end.
