-module(parser_test).
-include_lib("eunit/include/eunit.hrl").

-import(dict_parser, [parse/1]).
-import(tries, [has_branch/2, get_branch/2, is_terminator/1]).
-define(TEST_FILE, "test/testdict.txt").

parse_file_test() ->
	TestingTrie = parse(?TEST_FILE),
	?assert(has_branch($P, TestingTrie)),
	?assert(has_branch($A, TestingTrie)),
	?assert(has_branch($N, TestingTrie)),
	?assert(has_branch($R, TestingTrie)),
	{_, Deeper} = get_branch($A, get_second(get_branch($P, TestingTrie))),
	?assert(has_branch($U, Deeper)),
	{_, End} = get_branch($L, get_second(get_branch($U, Deeper))),
	?assert(is_terminator(End)),
	?assert(has_branch($H, TestingTrie)).

get_second({_, Second}) -> Second.

