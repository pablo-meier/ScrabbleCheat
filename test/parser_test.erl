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

-module(parser_test).
-include_lib("eunit/include/eunit.hrl").

-import(dict_parser, [parse/1]).
-import(gaddag, [naive_path_search/2, has_word/2]).
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


with_care_test() ->
  	TestingGaddag = parse(?TEST_FILE),
	?assert(naive_path_search("C&ARE", TestingGaddag)),
	?assert(naive_path_search("AC&RE", TestingGaddag)),
	?assert(naive_path_search("RAC&E", TestingGaddag)),
	?assert(naive_path_search("ERAC&", TestingGaddag)),
	?assert(has_word("CARE", TestingGaddag)).

parse_fail_test() ->
	?assertException(throw, {file_not_found, "Fakey McFakerson"}, parse("Fakey McFakerson")).

