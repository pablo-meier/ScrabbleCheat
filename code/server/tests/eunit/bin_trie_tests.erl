%% Copyright (c) 2010 Paul Meier %% 
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

-module(bin_trie_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_KEYS, [$A, $D, $E, $G, $Z]).
-define(TEST_BINTRIE, {ticket, <<5,
						$A,21,00,00,00,
						$D,32,00,00,00,
						$E,43,00,00,00,
						$G,54,00,00,00,
						$Z,82,00,00,00,
						255>>, twl06}).

start_gen_server() ->
    case whereis(giant_bintrie) of
        undefined -> ok;
        Else -> unregister(giant_bintrie)
    end,
    bin_trie:start_with_file("../ebin/testdict.dict").

end_gen_server() ->
    case whereis(giant_bintrie) of
        undefined -> ok;
        Else -> unregister(giant_bintrie),
                exit(Else, "Test finished")
    end.

is_key_test() ->
    Test = ?TEST_BINTRIE,
    ?assert(bin_trie:is_key($A, Test)),
    ?assert(bin_trie:is_key($L, Test) =:= false),
    ?assertException(throw, {out_of_trie_range, 300}, bin_trie:is_key(300, Test)).


is_terminator_test() ->
	?assert(bin_trie:is_terminator( ?TEST_BINTRIE )).


erase_test() ->
    Test = ?TEST_BINTRIE,

    ?assert(bin_trie:is_key($A, Test)),
    WithoutA = bin_trie:erase($A, Test),
    ?assert(bin_trie:is_key($A, WithoutA) =:= false),
    Keys = [$E, $D, $G, $Z],
    lists:foreach(fun (X) -> ?assert(bin_trie:is_key(X, WithoutA)) end, Keys),

    ?assert(bin_trie:is_key($L, Test) =:= false),
    WithoutL = bin_trie:erase($L, Test),
    ?assert(bin_trie:is_key($L, WithoutL) =:= false),

    {ticket, Bin, _} = WithoutL,
    {ticket, ExpectedBin, _} = Test,
    ?assert(byte_size(Bin) =:= byte_size(ExpectedBin)),
   
    ?assertException(throw, {out_of_trie_range, 300}, bin_trie:erase(300, Test)).


fetch_keys_test() ->
    ?assert(?TEST_KEYS =:= bin_trie:fetch_keys(?TEST_BINTRIE)).

