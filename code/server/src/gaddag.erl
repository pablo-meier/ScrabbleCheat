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

-export([has_branch/2, 
         get_branch/2, 
         delete_branch/2,
         has_word/2,
         is_terminator/1,
         keys/1,
         naive_path_search/2,
         get_branch_from_string/2]).

-define(WILDCARD,  $*).
-define(SEPARATOR, $&).


%% has_branch :: Char * Trie -> Bool
%%
%% Asks whether the subtree (GADDAG) contains a branch for the parameter.
has_branch(Char, Trie) ->
    bin_trie:is_key(Char, Trie).


%% delete_branch :: Char * Gaddag -> Gaddag
%%
%% Prunes a key from a gaddag, returning the gaddag with all other keys intact.
%% Assumes the key exists, crashes otherwise.
delete_branch(Key, Gaddag) ->
    bin_trie:erase(Key, Gaddag).


%% get_branch :: Char * Trie -> Trie
%%
%% Gets the specified branch, or 'none' if it doesn't exist.
get_branch(Char, Trie) ->
    Result = bin_trie:find(Char, Trie),
    case Result of
        {branch_not_found, _Key} -> none;
        Return -> {branch, Return}
    end.


%% keys :: Gaddag -> [Char]
%%
%% Returns all the keys this Gaddag has.
keys(Gaddag) ->
    lists:filter(fun (X) -> X =/= [?SEPARATOR] end, bin_trie:fetch_keys(Gaddag)).


%% has_word :: String * Trie -> Bool
%%
%% Returns whether or not the Trie contains a word.  We do this by removing the
%% first letter and separator, and doing a naive path search on the rest of the
%% word.
has_word([], _) -> false;
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
naive_path_search([], Gaddag) -> 
    is_terminator(Gaddag);

naive_path_search([FirstChar|Rest], Gaddag) ->
    case get_branch(FirstChar, Gaddag) of
        none -> 
            false;
        {branch, NextGaddag} ->
            naive_path_search(Rest, NextGaddag)
    end.


%% is_terminator :: Trie -> Bool
%%
%% Determines whether or not a word can end on this sub-GADDAG.
is_terminator(Gaddag) ->
	bin_trie:is_terminator(Gaddag).



%% get_branch_from_string :: Sting * Gaddag -> Gaddag
%%
%% Like an extended get_branch, though without the intermediate {branch, _} values.
%% Allows you to 'follow-through' an entire tree.  Mostly for testing.
get_branch_from_string([], Gaddag) -> Gaddag;
get_branch_from_string([H|T], Gaddag) -> 
    {branch, Follow} = get_branch(H, Gaddag),
    get_branch_from_string(T, Follow).

