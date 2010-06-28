-module(wordsearch_test).
-include_lib("eunit/include/eunit.hrl").

-import(wordsearch, [make_word_function/1]).
-import(dict_parser, [parse/1]).
-define(TEST_FILE, "test/smalldict.txt").

get_fun() ->
	make_word_function(parse(?TEST_FILE)).

get_compare_list() ->
	{"CA*", ["ACE","ACT","ARC","CAB","CAD","CAM","CAN","CAP","CAR","CAT","CAW","CAY","LAC","MAC",
	 		 "OCA","PAC","SAC","VAC","AA","AB","AD","AE","AG","AH","AI","AL","AM","AN",
             "AR","AS","AT","AW","AX","AY","BA","FA","HA","KA","LA","MA","NA","PA",
			 "TA","YA","ZA"]}.

simple_test() ->
	Search = get_fun(),
	Words = Search("dog"),
	?assert(lists:member("DOG", Words)),
	?assert(lists:member("GOD", Words)).

wildcard_test() ->
	Search = get_fun(),
	Words = Search("A*"),
	io:format("~p~n", [Words]),
	?assert(lists:member("AS", Words)),
	?assert(lists:member("HA", Words)),
	
	run_pattern_on_list(get_compare_list(), Search).


run_pattern_on_list({Pattern, List}, Search) ->
	MoreWords = Search(Pattern),
	lists:map(fun (X) -> ?assert(lists:member(X, List)) end, List).
