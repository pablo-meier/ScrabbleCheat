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

-module(main).
-import(movesearch, [make_word_function/1]).
-import(dict_parser, [parse/1]).
-import(lists, [reverse/1,foreach/2, keysort/2, sort/2, map/2]).
-define(DICT_FILE, "lib/twl06.txt").
-export([main/0]).


%% Main program. Currently will simply accept strings and show you permutations.

%% main :: () -> IO ()
%% Test
main() ->
	greet(),
	Trie = dict_parser:parse(?DICT_FILE),
	Word_Function = make_word_function(Trie),
	loop(Word_Function).


%% loop :: ([Char] -> [String]) -> IO ()
loop(Search) ->
	Chars = prompt(),
	Results = Search(Chars),
	print_results(order_results(Results)),
	case use_again() of
		true -> loop(Search);
		false -> 
			io:format("Thanks!~n"),
			erlang:halt()
	end.

%% print_results :: [{String, Length, RawScore}] -> IO ()
print_results(ResultList) ->
	foreach(fun (X) -> {W, _, _} = X, io:format("~p~n", [W]) end, ResultList).

%% order_results :: [String] -> [String]
order_results(ResultList) ->
	WithLength = map(fun (X) -> {X, length(X), score(X)} end, ResultList),
	reverse(keysort(3, reverse(keysort(2, WithLength)))).





%% Calculates the score of a string, without bonuses applied.
score(String) ->
	length(String).




greet() ->
	io:format("--------~nWelcome to ScrabbleCheat!~n~n").

prompt() ->
	io:get_line("Enter the letters you'd like to find words for: ").

use_again() ->
	Answer = io:get_line("Would you like to submit again? [y/n]:  "),
	case re:run(Answer, "[yY]([Ee][sS])?") of
		{match, _Captured} -> true;
		nomatch -> false
	end.
