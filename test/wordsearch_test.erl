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

empty_test() ->
	Search = get_fun(),
	Words = Search(""),
	?assert(length(Words) =:= 0).

run_pattern_on_list({Pattern, List}, Search) ->
	MoreWords = Search(Pattern),
	lists:map(fun (X) -> ?assert(lists:member(X, List)) end, MoreWords).
