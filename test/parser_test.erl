-module(parser_test).
-include_lib("eunit/include/eunit.hrl").

-import(dict_parser, [parse/1]).
-import(gaddag, [naive_path_search/2]).
-define(TEST_FILE, "test/testdict.txt").


parse_file_test() ->
  	TestingGaddag = parse(?TEST_FILE),
	?assert(naive_path_search("P&AUL",TestingGaddag)),
	?assert(naive_path_search("AP&UL",TestingGaddag)),
	?assert(naive_path_search("UAP&L",TestingGaddag)),
	?assert(naive_path_search("LUAP&",TestingGaddag)),
	?assert(naive_path_search("R&OBERT",TestingGaddag)),
	?assert(naive_path_search("OR&BERT",TestingGaddag)),
	?assert(naive_path_search("BOR&ERT",TestingGaddag)),
	?assert(naive_path_search("EBOR&RT",TestingGaddag)),
	?assert(naive_path_search("REBOR&T",TestingGaddag)),
	?assert(naive_path_search("TREBOR&",TestingGaddag)),
	?assert(naive_path_search("ON&LINEENDING",TestingGaddag)),
	?assert(naive_path_search("ILON&NEENDING",TestingGaddag)),
	?assert(naive_path_search("ILANNA&SA",TestingGaddag)).

parse_fail_test() ->
	?assertException(throw, {file_not_found, "Fakey McFakerson"}, parse("Fakey McFakerson")).

