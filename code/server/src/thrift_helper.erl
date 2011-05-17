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
-include("gameinfo.hrl").

-export([gamestate_to_thrift/1,
         thrift_to_gamestate/1,

         gameinfo_to_thrift/1,

         as_native_game/1,
         as_native_dict/1,

         %% I don't really want to export these, but the tests need them :-/
         %% TODO ifdef a test build?  boo bifurcation... :-(
         native_to_thrift_tile/1,
         thrift_to_native_tile/1,
         native_to_thrift_board/1,
         thrift_to_native_board/1]).


%% gamestate_to_thrift :: Gamestate -> ThriftGamestate
%%
%% Converts one of our gamestates to a Thrift gamestate. Because
%% I'm a boss, I use the literal syntax for the records rather than
%% record#{} syntax, since we can't resolve these values at compile-time
%% and need to generate the same thing dynamically.  Smelly smelly...
gamestate_to_thrift(Gamestate) ->
    Board   = gamestate:get_gamestate_board(Gamestate),
    Scores  = gamestate:get_gamestate_scores(Gamestate),
    Turn    = gamestate:get_gamestate_turn(Gamestate),
    History = gamestate:get_gamestate_history(Gamestate),
    Game    = gamestate:get_gamestate_game(Gamestate),
    Dict    = gamestate:get_gamestate_dict(Gamestate),


    ThriftBoard = native_to_thrift_board(Board),
    ThriftScores = dict:from_list(Scores),
    ThriftTurnOrder = lists:map(fun ({Name,_}) -> Name end, Scores),
    ThriftHistory = lists:map(fun ({Name, Move, Score}) ->
                                  MoveTiles = move:get_move_tiles(Move),
                                  ThriftTileList = lists:map(fun native_to_thrift_tile/1, MoveTiles),
                                  ThriftMove = {move, ThriftTileList, Score},
                                  {turn, ThriftMove, Name}
                              end, History),
    ThriftGame = as_thrift_game(Game),
    ThriftDict = as_thrift_dict(Dict),

    %% Due to the fact that records are a compile time hack, the compiler 
    %% complains if we make them using record syntax with dynamic data
    %% (halting problem, natch). Internal representation ftw...
    {gamestate, ThriftBoard, 
                ThriftScores, 
                Turn, 
                ThriftTurnOrder, 
                ThriftHistory,
                ThriftGame,
                ThriftDict}.


%% thrift_to_gamestate :: ThriftGamestate -> Gamestate
%%
%% Converts a Thrift gamestate into one of ours.
thrift_to_gamestate(#gamestate{board       = Board, 
                               scores      = Scores, 
                               player_turn = Turn, 
                               turn_order  = Order, 
                               history     = History,
                               game_name   = Game,
                               dict        = Dict}) ->
    NativeBoard = thrift_to_native_board(Board),
    NativeScores = lists:map(fun (Curr) -> 
                                 Name = binary_to_list(Curr),
                                 Score = dict:fetch(Curr, Scores),
                                 {Name, Score}
                             end, Order),
    NativeTurn = binary_to_list(Turn),
    NativeHistory = lists:map(fun (X) ->
                                  {turn, {move, Tiles, Score}, NameBin} = X,
                                  Name = binary_to_list(NameBin),
                                  TileList = lists:map(fun thrift_to_native_tile/1, Tiles),
                                  Move = move:from_list(TileList),
                                  {Name, Move, Score}
                              end, History),
    NativeGame = as_native_game(Game),
    NativeDict = as_native_dict(Dict),
    gamestate:make_gamestate(NativeBoard, 
                             NativeScores, 
                             NativeTurn, 
                             NativeHistory,
                             NativeGame,
                             NativeDict).

%% gameinfo_to_thrift :: Gameinfo -> ThriftGameinfo
%%
%% Converts one of our native Gameinfo structures to a thrift version, for
%% passing back to clients.
gameinfo_to_thrift(GI) ->
    Name       = GI#gameinfo.name,
    Board      = GI#gameinfo.board,
    LetterDist = GI#gameinfo.letterdist,
    ScoreDist  = GI#gameinfo.scoredist,
    RackSize   = GI#gameinfo.racksize,
    Bingo      = GI#gameinfo.bingo_bonuses,
    Dicts      = GI#gameinfo.allowed_dicts,
    
    ThriftName = as_thrift_game(Name),
    ThriftBoard = native_to_thrift_board(Board), 
    ThriftLetterDist = transform_keys_to_strings(LetterDist),
    ThriftScoreDist = transform_keys_to_strings(ScoreDist),
    ThriftDicts = lists:map(fun as_thrift_dict/1, Dicts),
    {gameInfo, ThriftName, 
               RackSize,
               Bingo,
               ThriftLetterDist,
               ThriftScoreDist,
               ThriftDicts,
               ThriftBoard}.

%% We pass around chars for letters that can occupy a tile, but Thrift
%% expects strings. This takes a dict from Char -> Int and makes it
%% String -> Int.
transform_keys_to_strings(Dict) ->
    AsList = dict:to_list(Dict),
    Transformed = lists:map(fun({K, V}) -> {list_to_binary([K]), V} end, AsList),
    dict:from_list(Transformed).

thrift_to_native_board(ThriftBoard) ->
    board:from_list(lists:map(fun thrift_to_native_tile/1, ThriftBoard)).

native_to_thrift_board(Board) ->
    lists:map(fun native_to_thrift_tile/1, lists:flatten(board:as_list(Board))).


native_to_thrift_tile(Tile) ->
    {Row, Col} = tile:get_tile_location(Tile),
    Bonus = as_thrift_bonus(tile:get_tile_bonus(Tile)),
    Letter = as_thrift_letter(tile:get_tile_letter(Tile)),
    LetterType = as_thrift_letter_type(tile:get_tile_letter_type(Tile)),
    
    #tile{row=Row, col=Col, type=LetterType, letter=Letter, bonus=Bonus}.


thrift_to_native_tile(#tile{row=Row, col=Col, type=LetterType, letter=Letter, bonus=Bonus}) ->
    NativeBonus = as_native_bonus(Bonus),
    NativeType = as_native_letter_type(LetterType),
    NativeLetter = as_native_letter(Letter),
    case NativeType of
        none -> tile:new_tile(none, NativeBonus, Row, Col);
        _Else ->
            tile:new_tile({NativeType, NativeLetter}, NativeBonus, Row, Col)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% LETTERS
as_native_letter(<<"">>) -> none;
as_native_letter(Else) -> hd(binary_to_list(Else)).

as_thrift_letter(none) -> "";
as_thrift_letter(Else) -> <<Else>>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Bonuses
as_thrift_bonus(triple_word_score)   -> ?scrabbleCheat_Bonus_TRIPLE_WORD_SCORE;
as_thrift_bonus(double_word_score)   -> ?scrabbleCheat_Bonus_DOUBLE_WORD_SCORE;
as_thrift_bonus(triple_letter_score) -> ?scrabbleCheat_Bonus_TRIPLE_LETTER_SCORE;
as_thrift_bonus(double_letter_score) -> ?scrabbleCheat_Bonus_DOUBLE_LETTER_SCORE;
as_thrift_bonus(none)                -> ?scrabbleCheat_Bonus_NONE.

as_native_bonus(?scrabbleCheat_Bonus_TRIPLE_WORD_SCORE)   -> triple_word_score;
as_native_bonus(?scrabbleCheat_Bonus_DOUBLE_WORD_SCORE)   -> double_word_score;
as_native_bonus(?scrabbleCheat_Bonus_TRIPLE_LETTER_SCORE) -> triple_letter_score;
as_native_bonus(?scrabbleCheat_Bonus_DOUBLE_LETTER_SCORE) -> double_letter_score;
as_native_bonus(?scrabbleCheat_Bonus_NONE)                -> none.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Letter Types
as_thrift_letter_type(wildcard)  -> ?scrabbleCheat_LetterType_WILDCARD;
as_thrift_letter_type(character) -> ?scrabbleCheat_LetterType_CHARACTER;
as_thrift_letter_type(none)      -> ?scrabbleCheat_LetterType_EMPTY.

as_native_letter_type(?scrabbleCheat_LetterType_WILDCARD)  -> wildcard;
as_native_letter_type(?scrabbleCheat_LetterType_CHARACTER) -> character;
as_native_letter_type(?scrabbleCheat_LetterType_EMPTY)     -> none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Game names
as_thrift_game(scrabble)           -> ?scrabbleCheat_GameName_SCRABBLE;
as_thrift_game(lexulous)           -> ?scrabbleCheat_GameName_LEXULOUS;
as_thrift_game(words_with_friends) -> ?scrabbleCheat_GameName_WORDS_WITH_FRIENDS;
as_thrift_game(Else)               -> throw({not_valid_native_game, Else}).

as_native_game(?scrabbleCheat_GameName_SCRABBLE)           -> scrabble;
as_native_game(?scrabbleCheat_GameName_LEXULOUS)           -> lexulous;
as_native_game(?scrabbleCheat_GameName_WORDS_WITH_FRIENDS) -> words_with_friends;
as_native_game(Else)                                       -> throw({not_valid_thrift_game, Else}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Dictionaries
as_thrift_dict(twl06)   -> ?scrabbleCheat_Dictionary_TWL06;
as_thrift_dict(sowpods) -> ?scrabbleCheat_Dictionary_SOWPODS;
as_thrift_dict(zynga)   -> ?scrabbleCheat_Dictionary_ZYNGA;
as_thrift_dict(Else)    -> throw({not_valid_native_dictionary, Else}).

as_native_dict(?scrabbleCheat_Dictionary_TWL06)   -> twl06;
as_native_dict(?scrabbleCheat_Dictionary_SOWPODS) -> sowpods;
as_native_dict(?scrabbleCheat_Dictionary_ZYNGA)   -> zynga;
as_native_dict(Else)                              -> throw({not_valid_thrift_dictionary, Else}).

