-module(movesearch).
-export([make_word_function/1]).
-import(string_utils, [format_string_for_gaddag/1]).
-import(lists, [flatmap/2, usort/1]).
-import(gaddag, [get_branch/2, is_terminator/1]).


%% make_word_function :: Trie -> ([Char] -> [String])
%%
%% Creates a function that uses the parametrized Trie to build a search
%% function that returns a list of words that can be formed from a list of letters.
make_word_function(Trie) ->
	fun (Letters) ->
		Upcased = format_string_for_gaddag(Letters),
		Words = flatmap(fun (X) -> find_all_words(X, lists:delete(X, Upcased), Trie) end, Upcased),
		usort(Words)		
	end.

%% find_all_words :: Char * [Char] * Trie -> [String]
%%
%% Traverses the parametrized Trie and finds all words that can
%% be built with the following letters.
find_all_words(Char, Remaining, Trie) ->
	find_all_words(Char, Remaining, [], [], Trie).

find_all_words(Char, [], Curr_Word, Accum, Trie) ->
	Result = get_branch(Char, Trie),
	case Result of
		none ->
			Accum;
		{wildcard, BranchList} ->
			flatmap(fun ({Key, Branch}) -> 
						Terminate = make_termination_clause(Curr_Word, Accum),
						Terminate(Key, Branch)
					end, BranchList);
		{branch, Branch} ->
			Terminate = make_termination_clause(Curr_Word, Accum),
			Terminate(Char, Branch)
	end;

find_all_words(Char, Remaining, Curr_Word, Accum, Trie) ->
	Result = get_branch(Char, Trie),
	case Result of
		none ->
			Accum;
		{wildcard, BranchList} ->
			flatmap(fun ({Key, BranchOfMany}) -> 
						SearchFun = get_branch_search_function(Key, Curr_Word, Remaining, Accum),
						flatmap(fun (X) -> SearchFun(X, BranchOfMany) end, Remaining)
					end, BranchList);
		{branch, Branch} ->
			SearchFun = get_branch_search_function(Char, Curr_Word, Remaining, Accum),
			flatmap(fun (X) -> SearchFun(X, Branch) end, Remaining)
	end.


%% get_branch_search_function :: (Closure-Vars) -> (Char * Trie -> [String])
%% Allows us to build a search function with that varies only on the Character
%% investigated and branches traversed.
get_branch_search_function(Char, Curr_Word, Remaining, Accum) ->
	fun (X, Branch) ->
		Termination = is_terminator(Branch),
		case Termination of
			true ->
				NewWord = [Char|Curr_Word],
				Accumulated = [lists:reverse(NewWord)|Accum],
				find_all_words(X, lists:delete(X, Remaining), NewWord, Accumulated, Branch);
			_False ->
				NewWord = [Char|Curr_Word],
				find_all_words(X, lists:delete(X, Remaining), NewWord, Accum, Branch)
		end
	end.

%% make_termination_clause :: ([Char] * [String]) -> (Char * Trie -> [String])
%% Similar to above, allows us to not repeat ourselves when searching for
%% the final combination by abstracting what happens into a function.
make_termination_clause(Curr_Word, Accum) ->
	fun (Char, Branch) ->
		case is_terminator(Branch) of
			true ->
				NewWord = [Char|Curr_Word],
				[lists:reverse(NewWord)|Accum];
			_False ->
				Accum
		end
	end.

	
