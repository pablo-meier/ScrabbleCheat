-module(gaddag_test).  
-include_lib("eunit/include/eunit.hrl").

-import(gaddag, [empty_gaddag/0, 
			 	add_word/2, 
				is_terminator/1, 
				has_branch/2, 
				get_branch/2,
				has_word/2,
				naive_path_search/2]).

get_fixture_gaddag() ->
	Words = ["ow", "wow", "paulie", "ox", "pamper", "wizard"],
	lists:foldl(fun gaddag:add_word/2, empty_gaddag(), Words).

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

search_test() -> 
	Gaddag = get_fixture_gaddag(),
	?assert(has_word("pamper", Gaddag)),
	?assert(has_word("wizard", Gaddag)),
	?assert(has_word("ox", Gaddag)),
	?assert(has_word("paulie", Gaddag)).

one_letter_word_test() ->
	Gaddag = get_fixture_gaddag(),
	?assert(is_terminator(Gaddag) =:= false).

representations_test() ->
	Words = ["silly", "putty", "as"],
	Gaddag = lists:foldl(fun gaddag:add_word/2, empty_gaddag(), Words),
	Rep1 = ["s&illy","is&lly", "lis&ly", "llis&y", "yllis&"],
	Rep2 = ["p&utty", "up&tty", "tup&ty", "ttup&y", "yttup&"],
	Rep3 = ["a&s", "sa&"],
	Representations = Rep1 ++ Rep2 ++ Rep3,
	lists:all(fun (X) -> naive_path_search(X, Gaddag) end, Representations).
