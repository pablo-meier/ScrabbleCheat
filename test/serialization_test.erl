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

-module(serialization_test).
-include_lib("eunit/include/eunit.hrl").

-import(serialization, [serialize_list/2, deserialize_list/2, split_with_delimeter/2]).


serialize_list_first_test() ->
    MyList = [paul, robert, annalisa, forrester],
    Serialized = serialize_list(MyList, fun atom_to_list/1),
    ?assert(Serialized == "paul|robert|annalisa|forrester|"),
    Backagain = deserialize_list(Serialized, fun list_to_atom/1),
    ?assert(MyList == Backagain).


serialize_list_empty_test() ->
    Serialized = serialize_list([], fun (X) -> X end),
    ?assert(Serialized == []),
    Backagain = deserialize_list(Serialized, fun list_to_atom/1),
    ?assert([] == Backagain).


split_with_delimeter_test() ->
    TestString = "ABC#DEF#GHI",
    {First, Rest} = split_with_delimeter(TestString, $#),
    ?assert(First == "ABC" andalso Rest == "DEF#GHI"),
    {More, Tail} = split_with_delimeter(Rest, $#),
    ?assert(More == "DEF" andalso Tail == "GHI").


