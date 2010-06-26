-module(wordsearch).
-export([test_program/0, make_word_function/1]).
-define(DICT_FILE, "lib/twl06.txt").
-import(string_utils, [format_string_for_trie/1]).
-import(lists, [flatmap/2, usort/1]).
-import(tries, [has_branch/2, get_branch/2, is_terminator/1]).


%% main :: () -> ()
%% Test
test_program() ->
	Trie = dict_parser:parse(?DICT_FILE),
	Word_Function = make_word_function(Trie),
	List_Of_Words = Word_Function("dog"),
	io:format("~p~n", [List_Of_Words]).

%% make_word_function :: Trie -> ([Char] -> [String])
%%
%% Creates a function that uses the parametrized Trie to build a search
%% function that returns a list of words that can be formed from a list of letters.
make_word_function(Trie) ->
	fun (Letters) ->
		Upcased = format_string_for_trie(Letters),
		Words = flatmap(fun (X) -> find_all_words(X, lists:delete(X, Upcased), Trie) end, Upcased),
		usort(Words)		
	end.

%% find_all_words :: Char * [Char] * Trie -> [String]
%%
%% Traverses the parametrized Trie and finds all words that can
%% be built with the following letters.
find_all_words(Char, Remaining, Trie) ->
	io:format("Starting!~n"),
	find_all_words(Char, Remaining, [], [], Trie).

find_all_words(Char, [], Curr_Word, Accum, Trie) ->
	io:format("End of the line!  Last checks with ~p, building on ~p~n", [[Char], lists:reverse(Curr_Word)]),
	Result = get_branch(Char, Trie),
	case Result of
		none ->
			io:format("   No branch, alas!~n"),
			Accum;
		Branch ->
			io:format("  Does branch, but terminate?~n"),
			Termination = is_terminator(Branch),
			case Termination of
				true ->
					NewWord = [Char|Curr_Word],
					io:format("  Aye!  Check out ~p~n", [NewWord]),
					[lists:reverse(NewWord)|Accum];
				_False ->
					io:format("  As the horse says, NAAAAY~n"),
					Accum
			end
	end;
	
find_all_words(Char, [Fst|Rst], Curr_Word, Accum, Trie) ->
	io:format("Checking ~p, building on ~p~n", [[Char], lists:reverse(Curr_Word)]),
	Result = get_branch(Char, Trie),
	case Result of
		none ->
			io:format("No branch for ~p~n", [[Char]]),
			Accum;
		Branch ->
			io:format("Branch found!~n"),
			Termination = is_terminator(Branch),
			case Termination of
				true ->
					NewWord = [Char|Curr_Word],
					io:format("Terminator! New Word is ~p~n", [NewWord]),
					find_all_words(Fst, Rst, NewWord, [lists:reverse(NewWord)|Accum], Branch);
				_False ->
					NewWord = [Char|Curr_Word],
					io:format("No Terminator! Building up with ~p~n", [NewWord]),
					find_all_words(Fst, Rst, NewWord, Accum, Branch)
			end
	end.




