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

-module(thrift_helper).

-include("scrabbleCheat_thrift.hrl").

-import(gamestate, [get_gamestate_board/1,
                    get_gamestate_scores/1,
                    get_gamestate_turn/1,
                    get_gamestate_history/1,
                    make_gamestate/4]).

-export([gamestate_to_thrift/1,
         thrift_to_gamestate/1]).


%% gamestate_to_thrift :: Gamestate -> ThriftGamestate
%%
%% Converts one of our gamestates to a Thrift gamestate. Because
%% I'm a boss, I use the literal syntax for the records rather than
%% record#{} syntax, since we can't resolve these values at compile-time
%% and need to generate the same thing dynamically.  Smelly smelly...
gamestate_to_thrift(Gamestate) ->
    Board = get_gamestate_board(Gamestate),
    Scores = get_gamestate_scores(Gamestate),
    Turn = get_gamestate_turn(Gamestate),
    History = get_gamestate_history(Gamestate),

    ThriftBoard = lists:map(fun native_to_thrift_tile/1, lists:flatten(board:as_list(Board))),
    ThriftScores = dict:from_list(Scores),
    ThriftTurnOrder = lists:map(fun ({Name,_}) -> Name end, Scores),
    ThriftHistory = lists:map(fun ({Name, Move, Score}) ->
                                  MoveList = lists:map(fun native_to_thrift_tile/1, Move),
                                  ThriftMove = {move, {MoveList, Score}},
                                  {turn, ThriftMove, Name}
                              end, History),
    {gamestate, ThriftBoard, ThriftScores, Turn, ThriftTurnOrder, ThriftHistory}.


%% thrift_to_gamestate :: ThriftGamestate -> Gamestate
%%
%% Converts a Thrift gamestate into one of ours.
thrift_to_gamestate(#gamestate{board = _Board, 
                               scores = _Scores, 
                               player_turn = _Turn, 
                               turn_order = _Order, 
                               history = _History}) ->
    ok.

native_to_thrift_tile(Tile) ->
    {Row, Col} = tile:get_tile_location(Tile),
    Bonus = as_thrift_bonus(tile:get_tile_bonus(Tile)),
    Letter = as_thrift_letter(tile:get_tile_letter(Tile)),
    LetterType = as_thrift_letter_type(tile:get_tile_letter_type(Tile)),
    
    #tile{row=Row, col=Col, type=LetterType, letter=Letter, bonus=Bonus}.


as_thrift_bonus(triple_word_score)   -> ?scrabbleCheat_Bonus_TRIPLE_WORD_SCORE;
as_thrift_bonus(double_word_score)   -> ?scrabbleCheat_Bonus_DOUBLE_WORD_SCORE;
as_thrift_bonus(triple_letter_score) -> ?scrabbleCheat_Bonus_TRIPLE_LETTER_SCORE;
as_thrift_bonus(double_letter_score) -> ?scrabbleCheat_Bonus_DOUBLE_LETTER_SCORE;
as_thrift_bonus(none)                -> ?scrabbleCheat_Bonus_NONE.


as_thrift_letter(none) -> "";
as_thrift_letter(Else) -> Else.

as_thrift_letter_type(wildcard) -> ?scrabbleCheat_LetterType_WILDCARD;
as_thrift_letter_type(character) -> ?scrabbleCheat_LetterType_CHARACTER;
as_thrift_letter_type(none) -> ?scrabbleCheat_LetterType_EMPTY.



as_native_bonus(?scrabbleCheat_Bonus_TRIPLE_WORD_SCORE)   -> triple_word_score;
as_native_bonus(?scrabbleCheat_Bonus_DOUBLE_WORD_SCORE)   -> double_word_score;
as_native_bonus(?scrabbleCheat_Bonus_TRIPLE_LETTER_SCORE) -> triple_letter_score;
as_native_bonus(?scrabbleCheat_Bonus_DOUBLE_LETTER_SCORE) -> double_letter_score;
as_native_bonus(?scrabbleCheat_Bonus_NONE)                -> none.

as_native_letter("") -> none;
as_native_letter(Else) -> Else.

as_native_letter_type(?scrabbleCheat_LetterType_WILDCARD) -> wildcard;
as_native_letter_type(?scrabbleCheat_LetterType_CHARACTER) -> character;
as_native_letter_type(?scrabbleCheat_LetterType_EMPTY) -> none.

