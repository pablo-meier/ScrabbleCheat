-module(gaddag_test).  -include_lib("eunit/include/eunit.hrl").

-import(gaddag, [empty_gaddag/0, 
			 	add_word/2, 
				is_terminator/1, 
				has_branch/2, 
				get_branch/2,
				has_word/2]).

get_fixture_gaddag() ->
	EmptyTrie = empty_gaddag(),
	New1 = add_word("ow", EmptyTrie),
	New2 = add_word("wow", New1),
	add_word("ox", New2).

has_branch_test() ->
	Trie = get_fixture_gaddag(),
	?assert(has_branch($o, Trie)),
	?assert(has_branch($w, Trie)).

get_branch_test() ->
	Trie = get_fixture_gaddag(),
	{branch, Trie1} = get_branch($w, Trie),
	{branch, Trie2} = get_branch($o, Trie),
	?assert(has_branch($o, Trie1)),
	?assert(has_branch($x, Trie2)).
	
adding_test() ->
	Trie = get_fixture_gaddag(),
	New_Trie = add_word("always", Trie),
	?assert(has_branch($a, New_Trie)),
	{branch, Sub_Trie} = get_branch($a, New_Trie),
	?assert(has_branch($l, Sub_Trie)).

word_search_test() ->
	Trie = get_fixture_gaddag(),
	?assert(has_word("wow", Trie)),
	?assert(has_word("ox", Trie)),
	?assert(has_word("paulpaulpaul", Trie) =:= false).

