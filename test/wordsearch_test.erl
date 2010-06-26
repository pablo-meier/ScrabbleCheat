-module(wordsearch_test).
-include_lib("eunit/include/eunit.hrl").

-import(main, [make_word_function/1]).
-import(dict_parser, [parse/1]).
-define(TEST_FILE, "test/smalldict.txt").

get_fun() ->
	make_word_function(parse(?TEST_FILE)).

simple_test() ->
	Search = get_fun(),
	Words = Search("dog"),
	io:format("Search on \"dog\" returns ~p~n", [Words]),
	?assert(lists:member("DOG", Words)),
	?assert(lists:member("GOD", Words)).
