-module(tries).
-export([add_word/2, empty_trie/0, has_branch/2, get_branch/2, is_terminator/1]).

-import(gb_trees, [empty/0, lookup/2, is_defined/2, enter/3]).


%% Mostly a wrapper for gb_tries, allowing me to sub out if I like.
%% The atoms |terminator| and |blank_tile| are used rather than a macro.


%% add_word :: String * Trie -> Trie
add_word([], Trie) -> enter(terminator, dummy, Trie);
add_word(Word, Trie) ->
	[Char|Rest] = Word,
	case has_branch(Char, Trie) of	
		true ->
			Sub_Trie = get_branch(Char, Trie),
			New_Trie = add_word(Rest, Sub_Trie),
			enter(Char, New_Trie, Trie);
		_False ->
			Empty_Trie = empty(),
			New_Trie = add_word(Rest, Empty_Trie),
			enter(Char, New_Trie, Trie)
	end.


%% has_branch :: Char * Trie -> Bool
has_branch(Char, Trie) ->
	is_defined(Char, Trie).

empty_trie() ->
	gb_trees:empty().

%% get_branch :: Char * Trie -> Trie
get_branch(Char, Trie) ->
	{value, Return} = lookup(Char, Trie),
	Return.

%% is_terminator :: Trie -> Bool
is_terminator(Trie) ->
	has_branch(terminator, Trie).

