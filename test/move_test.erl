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

-import(move, [duplicate_moves/2, 
               score/2, 
               new_move/0, 
               add_to_move/2,
               serialize/1,
               deserialize/1]).

-import(lists, [foldl/3]).
-import(board, [place_word/4]).
-import(board_parser, [new_board/0]).

duplicate_move_1_test() ->
	Move1 = {move, [{{character, 67}, none, {6,7}},
					{{character, 82}, none, {6,10}},
					{{character, 91}, none, {6,11}}]},
	Move2 = {move, [{{character, 82}, none, {6,10}},
					{{character, 67}, none, {6,7}},
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
					{{character, 90}, none, {6,12}},
					{{character, 91}, none, {6,11}},
					{{character, 82}, none, {6,10}},
					{{character, 88}, none, {6,6}}]},

	?assert(duplicate_moves(Move1, Move3) =:= false),
	?assert(duplicate_moves(Move3, Move4) =:= false),
	?assert(duplicate_moves(Move4, Move1) =:= false),
	?assert(duplicate_moves(Move4, AlsoMove4)).

score_simple_test() ->
	Tiles = [{{character, $A}, double_letter_score, {7,7}}, 
			{{character, $B}, none, {7,8}},
			{{character, $L}, double_letter_score, {7,9}},
			{{character, $E}, none, {7,10}}],
	Move = foldl(fun move:add_to_move/2, new_move(), Tiles), 
	Score = score(Move, new_board()),
	io:format("Score is ~p~n", [Score]),
	?assert(Score =:= 8).

score_isolated_bonus_test() ->
	Tiles = [{{character, $A}, none, {8,7}}, 
			{{character, $B}, double_word_score, {8,8}},
			{{character, $L}, none, {8,9}},
			{{character, $E}, none, {8,10}}],
	Move = foldl(fun move:add_to_move/2, new_move(), Tiles), 
	Score = score(Move, new_board()),
	io:format("Score is ~p~n", [Score]),
	?assert(Score =:= 12).

score_parallel_test() ->
	Tiles = [{{character, $A}, triple_letter_score, {6,6}}, 
			{{character, $A}, none, {6,7}}],
	Move = foldl(fun move:add_to_move/2, new_move(), Tiles),
	Score = score(Move, place_word("ABLE", right, {5,6}, new_board())),
	io:format("Score is ~p~n", [Score]),
	?assert(Score =:= 12).
	

%% Should add 50 for a bingo.  Here we use AMEERATE, latching onto ABLE
score_bingos_test() ->
    Tiles = [{{character, $A}, none, {8,2}},
			 {{character, $M}, none, {8,3}},
			 {{character, $E}, double_letter_score, {8,4}},
			 {{character, $E}, none, {8,5}},
			 {{character, $R}, none, {8,6}},
			 {{character, $A}, none, {8,7}},
			 {{character, $T}, double_word_score, {8,8}}],
	Move = foldl(fun move:add_to_move/2, new_move(), Tiles),
	Score = score(Move, place_word("ABLE", down, {5,9}, new_board())),
	io:format("Score is ~p~n", [Score]),
	% 2(1 + 3 + 2 + 1 + 1 + 1 + 1 + 1) = 22
	?assert(Score =:= 72).


score_along_wall_test() ->
	Tiles = [{{character, $E}, triple_word_score, {15,15}}, 
			{{character, $L}, none, {15,14}},
			{{character, $B}, none, {15,13}},
			{{character, $A}, none, {15,12}}],
	Move = foldl(fun move:add_to_move/2, new_move(), Tiles),
	Score = score(Move, new_board()),
	io:format("Score is ~p~n", [Score]),
	?assert(Score =:= 18).

	
score_parallel_many_bonuses_test() ->
	Tiles = [{{character, $Z}, none, {8,3}}, 
			{{character, $Y}, double_letter_score, {8,4}},
			{{character, $G}, none, {8,5}},
			{{character, $O}, none, {8,6}},
			{{character, $T}, none, {8,7}},
			{{character, $E}, double_word_score, {8,8}}],
	Move = foldl(fun move:add_to_move/2, new_move(), Tiles), 
	Score = score(Move, place_word("ABLE", right, {7,7}, new_board())),
	io:format("Score is ~p~n", [Score]),
	?assert(Score =:= 56).

serialize_first_test() ->
    Tiles = [{{character, $A}, double_word_score, {11,12}},
            {{wildcard, $C}, none, {11, 15}},
            {{character, $X}, none, {11, 9}}],
    Move = lists:foldl(fun move:add_to_move/2, new_move(), Tiles),
    ?assert(move:deserialize(move:serialize(Move)) == Move).

