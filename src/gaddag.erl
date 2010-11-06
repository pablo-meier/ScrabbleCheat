%% Copyright (c) 2010 Paul Meier
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(gaddag).

-export([add_word/2, 
         empty_gaddag/0, 
         has_branch/2, 
         get_branch/2, 
         has_word/2,
         is_terminator/1,
         naive_path_search/2,
         keys/1,
         get_branch_from_string/2]).

-define(WILDCARD, $*).
-define(SEPARATOR, $&).

-import(gb_trees, [empty/0, lookup/2, is_defined/2, enter/3, to_list/1]).
-import(lists, [filter/2, reverse/1, foldl/3]).
-import(string, [concat/2]).

%% Mostly a wrapper for gb_trees, allowing me to sub out if I like.
%% The atom 'terminator' is used rather than a macro.



%% has_branch :: Char * Trie -> Bool
%%
%% Asks whether the subtree (GADDAG) contains a branch for the parameter.
has_branch(Char, Trie) ->
    is_defined(Char, Trie).


%% get_branch :: Char * Trie -> Trie
%%
%% Gets the specified branch, or 'none' if it doesn't exist.
get_branch(Char, Trie) ->
    Result = lookup(Char, Trie),
    case Result of
        {value, Return} -> {branch, Return};
        _Fail -> none
    end.


%% keys :: Gaddag -> [Char]
%%
%% Returns all the keys this Gaddag has.
keys(Gaddag) ->
    lists:filter(fun (X) -> X =/= terminator end, gb_trees:keys(Gaddag)).

%% has_word :: String * Trie -> Bool
%%
%% Returns whether or not the Trie contains a word.  We do this by removing the
%% first letter and separator, and doing a naive path search on the rest of the
%% word.
has_word([H|T], Trie) ->
    case get_branch(H, Trie) of
        {branch, Next} -> 
            case get_branch(?SEPARATOR, Next) of
                {branch, GaddagToSearch} -> 
                    naive_path_search(T, GaddagToSearch);
                _Else -> false
            end;
        none -> false
    end.


%% naive_path_search :: String * Trie -> Bool
%%
%% Exported primarily for testing, DO NOT USE IN PRODUCTION. Checks whether a
%% naive sequence of characters can be followed to completion on the Trie.
naive_path_search([], Gaddag) -> is_terminator(Gaddag);
naive_path_search([FirstChar|Rest], Gaddag) ->
    case get_branch(FirstChar, Gaddag) of
        none -> false;
        {branch, NextGaddag} ->
            naive_path_search(Rest, NextGaddag)
    end.

%% is_terminator :: Trie -> Bool
%%
%% Determines whether or not a word can end on this sub-GADDAG.
is_terminator(Gaddag) ->
    Terminator = has_branch(terminator, Gaddag), 
    Separator = has_branch(?SEPARATOR, Gaddag),
    case {Terminator, Separator} of
        {_, true} -> {branch, Path} = get_branch(?SEPARATOR, Gaddag),
                     is_terminator(Path);
        {true, _} -> true;
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
    NewPrefix = concat(Prefix, [hd(Suffix)]),
    NewSuffix = tl(Suffix),
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

%% get_branch_from_string :: Sting * Gaddag -> Gaddag
%%
%% Like an extended get_branch, though without the intermediate {branch, _} values.
%% Allows you to 'follow-through' an entire tree.  Mostly for testing.
get_branch_from_string([], Gaddag) -> Gaddag;
get_branch_from_string([H|T], Gaddag) -> 
    {branch, Follow} = get_branch(H, Gaddag),
    get_branch_from_string(T, Follow).
