-module(trie_test).
-include_lib("eunit/include/eunit.hrl").

-import(tries, [empty_trie/0, 
			 	add_word/2, 
				is_terminator/1, 
				has_branch/2, 
				get_branch/2,
				has_word/2]).

get_fixture_trie() ->
	EmptyTrie = empty_trie(),
	New1 = add_word("ow", EmptyTrie),
	New2 = add_word("wow", New1),
	add_word("ox", New2).

has_branch_test() ->
	Trie = get_fixture_trie(),
	?assert(has_branch($o, Trie)),
	?assert(has_branch($w, Trie)).

get_branch_test() ->
	Trie = get_fixture_trie(),
	{branch, Trie1} = get_branch($w, Trie),
	{branch, Trie2} = get_branch($o, Trie),
	?assert(has_branch($o, Trie1)),
	?assert(has_branch($x, Trie2)).
	
adding_test() ->
	Trie = get_fixture_trie(),
	New_Trie = add_word("always", Trie),
	?assert(has_branch($a, New_Trie)),
	{branch, Sub_Trie} = get_branch($a, New_Trie),
	?assert(has_branch($l, Sub_Trie)).

word_search_test() ->
	Trie = get_fixture_trie(),
	?assert(has_word("wow", Trie)),
	?assert(has_word("ox", Trie)),
	?assert(has_word("paulpaulpaul", Trie) =:= false).

