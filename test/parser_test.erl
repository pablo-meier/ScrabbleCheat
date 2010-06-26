-module(parser_test).
-include_lib("eunit/include/eunit.hrl").

-import(dict_parser, [parse/1]).
-define(DICT_FILE, "lib/twl06.txt").

parse_file_test() ->
	parse(?DICT_FILE),
	?assert(true).

