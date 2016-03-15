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

