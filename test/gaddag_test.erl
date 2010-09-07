-module(gaddag_test).  
-include_lib("eunit/include/eunit.hrl").

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
	New3 = add_word("paulie", New2),
	add_word("ox", New3).

has_branch_test() ->
	Trie = get_fixture_gaddag(),
	?assert(has_branch($o, Trie)),
	?assert(has_branch($w, Trie)),
	?assert(has_branch($e, Trie)),
	?assert(has_branch($u, Trie)).

get_branch_test() ->
	Trie = get_fixture_gaddag(),
	{branch, Trie1} = get_branch($w, Trie),
	{branch, Trie2} = get_branch($o, Trie),
	{branch, Trie3} = get_branch($u, Trie),
	?assert(has_branch($o, Trie1)),
	?assert(has_branch($&, Trie2)),
	?assert(has_branch($a, Trie3)).
	
adding_test() ->
	Trie = get_fixture_gaddag(),
	New_Trie = add_word("always", Trie),
	?assert(has_branch($y, New_Trie)),
	{branch, Sub_Trie} = get_branch($a, New_Trie),
	?assert(has_branch($w, Sub_Trie)).

