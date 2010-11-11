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

-module(serialization).
-import(lists, [map/2, concat/1]).

-export([serialize_list/2, deserialize_list/2, split_with_delimeter/2]).


%% serialize_list :: [a] * (a -> String) -> String
%%
%% Serializes a list of items according to the function passed in.  We use pipes to
%% separate list items "|".
serialize_list(Lst, Fun) ->
    concat(map(fun (X) -> concat([Fun(X), "|"]) end, Lst)).


%% deserialize_list :: String * (String -> a) -> [a]
%%
%% Takes a list of items created by serialize_list and forms it into a list of strings.
deserialize_list(ListString, Fun) ->
    Strings = list_deserialize_helper(ListString, []),
    map(Fun, Strings).

list_deserialize_helper([], Accum) -> lists:reverse(Accum);
list_deserialize_helper(String, Accum) -> 
    {Elem, Rst} = split_with_delimeter(String, $|),
    list_deserialize_helper(Rst, [Elem|Accum]).


%% split_with_delimeter :: String * Char -> {String, String}
%%
%% Splits a string into its left and right components by the parametrized delimeter.
split_with_delimeter(String, Char) ->
    {Left, [_|Right]} = lists:splitwith(fun (X) -> X =/= Char end, String),
    {Left, Right}.
