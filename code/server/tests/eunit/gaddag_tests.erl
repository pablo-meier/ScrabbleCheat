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

-module(gaddag_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEMP_FILE_NAME, "../ebin/gaddag_test.dict").

get_fixture_gaddag() ->
    bin_trie:get_root(twl06).
setup() ->
    bin_trie:start_from_file(?TEMP_FILE_NAME).
teardown() ->
    case whereis(giant_bintrie) of
        undefined -> ok;
        Else -> unregister(giant_bintrie),
                exit(Else, "Test finished")
    end.


has_branch_test() ->
    setup(),
    Gaddag = get_fixture_gaddag(),
    run_has_branch(Gaddag),
    teardown().

run_has_branch(Gaddag) ->
    ?assert(gaddag:has_branch($O, Gaddag)),
    ?assert(gaddag:has_branch($W, Gaddag)),
    ?assert(gaddag:has_branch($E, Gaddag)),
    ?assert(gaddag:has_branch($U, Gaddag)).
 

get_branch_test() ->
    setup(),
    Gaddag = get_fixture_gaddag(),
    run_get_branch(Gaddag),
    teardown().

run_get_branch(Gaddag) ->
    {branch, Trie1} = gaddag:get_branch($W, Gaddag),
    {branch, Trie2} = gaddag:get_branch($O, Gaddag),
    {branch, Trie3} = gaddag:get_branch($U, Gaddag),
    ?assert(gaddag:has_branch($O, Trie1)),
    ?assert(gaddag:has_branch($&, Trie2)),
    ?assert(gaddag:has_branch($A, Trie3)),
    ?assert(gaddag:has_branch($T, Trie3) =:= false),
    ?assert(gaddag:has_branch($T, Gaddag) =:= false).

 

search_test() -> 
    setup(),
    Gaddag = get_fixture_gaddag(),
    run_search(Gaddag),
    teardown().

run_search(Gaddag) ->
    ?assert(gaddag:has_word("PAMPER", Gaddag)),
    ?assert(gaddag:has_word("WIZARD", Gaddag)),
    ?assert(gaddag:has_word("OX", Gaddag)),
    ?assert(gaddag:has_word("PAULIE", Gaddag)),
    ?assert(gaddag:has_word("PAULI", Gaddag) =:= false),
    ?assert(gaddag:has_word("WIZ", Gaddag) =:= false),
    ?assert(gaddag:has_word("AMPER", Gaddag) =:= false).
 


one_letter_word_test() ->
    setup(),
    Gaddag = get_fixture_gaddag(),
    run_one_letter(Gaddag),
    teardown().

run_one_letter(Gaddag) ->
    ?assert(gaddag:is_terminator(Gaddag) =:= false).

