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

-module(gamestate_test).
-include_lib("eunit/include/eunit.hrl").

-import(board_parser, [new_board/0]).
-import(board, [place_word/4]).
-import(gamestate, [make_gamestate/4, serialize/1, deserialize/1]).


test_serialization_first_test() ->
	Board = place_word("ABLE", right, {7,7}, new_board()),
    Scores = [{"Paul", 100}, {"Sam", 50}],
    Turn = "Sam",
    Moves = [{"Paul", {move, [{{character, $X}, triple_word_score, {8,15}}, 
                              {{character, $O}, none, {9, 15}}, 
                              {{character, $R}, none, {10, 15}}]}, 100},
             {"Sam", {move, [{{character, $P}, triple_word_score, {8,11}}, 
                              {{character, $O}, none, {8, 9}},
                              {{character, $O}, none, {8, 10}}]}, 50}],
    Gamestate = make_gamestate(Board, Scores, Turn, Moves),
    ?assert(gamestate:deserialize(gamestate:serialize(Gamestate)) == Gamestate).


