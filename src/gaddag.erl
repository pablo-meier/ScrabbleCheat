-module(gaddag).

-export([add_word/2, 
		add_char_string/2,
		empty_gaddag/0, 
		has_branch/2, 
		get_branch/2, 
		is_terminator/1]).

-define(WILDCARD, $*).
-define(SEPARATOR, $&).

-import(gb_trees, [empty/0, lookup/2, is_defined/2, enter/3, to_list/1]).
-import(lists, [filter/2, reverse/1, foldl/3]).
-import(string, [concat/2]).

%% Mostly a wrapper for gb_trees, allowing me to sub out if I like.
%% The atom |terminator| is used rather than a macro.



%% has_branch :: Char * Trie -> Bool
%%
%% Asks whether the subtree (GADDAG) contains a branch for the parameter.
has_branch(Char, Trie) ->
	if
		Char =:= ?WILDCARD ->
			true;
		true ->
			is_defined(Char, Trie)
	end.


%% get_branch :: Char * Trie -> Trie
%%
%% Gets the specified branch, or 'none' if it doesn't exist.
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
%%
%% Determines whether or not a word can end on this sub-GADDAG.
is_terminator(Trie) ->
	HasTerminator = has_branch(terminator, Trie), 
	HasSeparator = has_branch(?SEPARATOR, Trie),
	Cases = {HasTerminator, HasSeparator},
	case Cases of
		{true, _} -> true;
		{_, true} -> is_terminator(get_branch(?SEPARATOR, Trie));
		_Else -> false
	end.



%% empty_gaddag :: () -> Trie
%%
%% Is the GADDAG empty?
empty_gaddag() ->
	gb_trees:empty().


%% add_word :: String * Trie -> Trie
%%
%% Returns a new GADDAG with all the prefix/suffix representations of a word represented.
add_word(String, Trie) ->
	foldl(fun (Rep, NewTrie) -> add_char_string(Rep, NewTrie) end, Trie, split_into_representations(String)).


%% split_into_representations :: String -> [String]
%%
%% Splits a single string into its multiple representations.
split_into_representations(Word) ->
	Results = splitter([], Word, []),
	Results.


%% splitter :: String * String * [String] -> [String]
splitter(_, [], Accum) -> Accum;
splitter(Prefix, Suffix, Accum) ->
	NewPrefix = concat(Prefix, [head(Suffix)]),
	NewSuffix = tail(Suffix),
	NewAddition = concat(concat(reverse(NewPrefix), [?SEPARATOR]), NewSuffix),
	splitter(NewPrefix, NewSuffix, [NewAddition|Accum]).
	

%% add_char_string :: String * Trie -> Trie
%%
%% Adds a string of characters (each a representation of a word) to the Trie, returns the new Trie.
add_char_string([], Trie) -> enter(terminator, dummy, Trie);
add_char_string(Word, Trie) ->
	[Char|Rest] = Word,
	case has_branch(Char, Trie) of	
		true ->
			{branch, Sub_Trie} = get_branch(Char, Trie),
			New_Trie = add_char_string(Rest, Sub_Trie),
			enter(Char, New_Trie, Trie);
		_False ->
			Empty_Trie = empty(),
			New_Trie = add_char_string(Rest, Empty_Trie),
			enter(Char, New_Trie, Trie)
	end.


head([H|_]) -> H.
tail([_|T]) -> T.
