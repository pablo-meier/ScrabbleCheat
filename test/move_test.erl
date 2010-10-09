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

-module(move_test).
-include_lib("eunit/include/eunit.hrl").

-import(move, [duplicate_moves/2]).

duplicate_move_1_test() ->
	Move1 = {move, [{{character, 67}, none, {6,7}},
					{{character, 82}, none, {6,10}},
					{{character, 91}, none, {6,11}}]},
	Move2 = {move, [{{character, 67}, none, {6,7}},
					{{character, 82}, none, {6,10}},
					{{character, 91}, none, {6,11}}]},
	?assert(duplicate_moves(Move1, Move2)),

	Move3 = {move, [{{character, 99}, none, {2,7}},
					{{character, 86}, none, {9,14}}]},

	Move4 = {move, [{{character, 67}, none, {6,7}},
					{{character, 82}, none, {6,10}},
					{{character, 91}, none, {6,11}},
					{{character, 90}, none, {6,12}},
					{{character, 88}, none, {6,6}}]},

	AlsoMove4 = {move, [{{character, 67}, none, {6,7}},
					{{character, 82}, none, {6,10}},
					{{character, 91}, none, {6,11}},
					{{character, 90}, none, {6,12}},
					{{character, 88}, none, {6,6}}]},

	?assert(duplicate_moves(Move1, Move3) =:= false),
	?assert(duplicate_moves(Move3, Move4) =:= false),
	?assert(duplicate_moves(Move4, Move1) =:= false),
	?assert(duplicate_moves(Move4, AlsoMove4)).

