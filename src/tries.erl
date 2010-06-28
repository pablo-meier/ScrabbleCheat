-module(tries).

-export([add_word/2, 
		has_word/2,
		empty_trie/0, 
		has_branch/2, 
		get_branch/2, 
		is_terminator/1]).

-define(WILDCARD, $*).

-import(gb_trees, [empty/0, lookup/2, is_defined/2, enter/3, to_list/1]).
-import(lists, [filter/2]).
%% Mostly a wrapper for gb_tries, allowing me to sub out if I like.
%% The atoms |terminator| and |blank_tile| are used rather than a macro.



%% has_word :: String * Trie -> Bool
has_word([], Trie) -> is_terminator(Trie);
has_word([Char|Rest], Trie) ->
	case get_branch(Char, Trie) of
		none -> false;
		{branch, Success} -> has_word(Rest, Success);
		{wildcard, BranchList} -> lists:any(fun(X) -> has_word(Rest, X) end, BranchList) 
	end.


%% has_branch :: Char * Trie -> Bool
has_branch(Char, Trie) ->
	if
		Char =:= ?WILDCARD ->
			true;
		true ->
			is_defined(Char, Trie)
	end.


%% get_branch :: Char * Trie -> Trie
get_branch(Char, Trie) ->
	if
		Char =:= ?WILDCARD ->
			{wildcard, filter(fun ({Key,_}) -> Key /= terminator end, to_list(Trie))};
		true ->
			Result = lookup(Char, Trie),
			case Result of
				{value, Return} -> {branch, Return};
				_Fail -> none
			end
	end.


%% is_terminator :: Trie -> Bool
is_terminator(Trie) ->
	has_branch(terminator, Trie).


%% empty_trie :: () -> Trie
empty_trie() ->
	gb_trees:empty().


%% add_word :: String * Trie -> Trie
add_word([], Trie) -> enter(terminator, dummy, Trie);
add_word(Word, Trie) ->
	[Char|Rest] = Word,
	case has_branch(Char, Trie) of	
		true ->
			{branch, Sub_Trie} = get_branch(Char, Trie),
			New_Trie = add_word(Rest, Sub_Trie),
			enter(Char, New_Trie, Trie);
		_False ->
			Empty_Trie = empty(),
			New_Trie = add_word(Rest, Empty_Trie),
			enter(Char, New_Trie, Trie)
	end.
