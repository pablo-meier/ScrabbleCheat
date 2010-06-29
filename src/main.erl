-module(main).
-import(wordsearch, [make_word_function/1]).
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
	10.




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
