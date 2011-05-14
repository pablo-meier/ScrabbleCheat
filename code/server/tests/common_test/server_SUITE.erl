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

-module(server_SUITE).
-include_lib("common_test/include/ct.hrl").

-include("eunit_macros.hrl").
-include("gameinfo.hrl").
-include("scrabbleCheat_thrift.hrl").

%% Test server callbacks.
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         suite/0]).

%% Test cases
-export([new_game_test/1,
         play_move_test/1,
         scrabblecheat_suggestions_test/1,
         bad_namelist_test/1,
         bad_rack_test/1,
         various_games_test/1,
         unallowed_dicts_test/1,
         bad_dicts_and_gamename_test/1,
         bad_board_test/1]).


-define(PORT, 8888).
-define(LOCALHOST, "127.0.0.1").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% COMMON TEST API

all() ->
    [new_game_test,
     play_move_test,
     scrabblecheat_suggestions_test,
     bad_namelist_test,
     bad_rack_test,
     various_games_test,
     unallowed_dicts_test,
     bad_dicts_and_gamename_test,
     bad_board_test].

suite() ->
    [{userdata, [{info, "Tests the top-level API, defined by the Thrift file."}]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok. % application:stop(scrabblecheat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% HELPER FUNCTIONS

get_thrift_client() ->
    case thrift_client_util:new(?LOCALHOST, ?PORT, scrabbleCheat_thrift, []) of
        {ok, ClientName} -> ClientName;
        Else -> throw({unexpected_client_value, Else})
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% LOL TESTS LOL

new_game_test(_Config) ->
    Client0 = get_thrift_client(),
    {_Client1, {ok, Gamestate}} = thrift_client:call(Client0, 
                                                     new_game, 
                                                     [["Paul", "Sam"], 
                                                      ?scrabbleCheat_GameName_SCRABBLE,
                                                      ?scrabbleCheat_Dictionary_TWL06]),
    #gamestate{board = _Board, scores = _Scores, player_turn = CurrentTurn} = Gamestate,
    #gamestate{turn_order = TurnOrder, history = History} = Gamestate,
    ?assert(string:equal(CurrentTurn, <<"Paul">>)),
    ?assert(length(History) =:= 0),
    ?assert(TurnOrder =:= [<<"Paul">>, <<"Sam">>]).


various_games_test(_Config) ->
    Client0 = get_thrift_client(),
    {Client1, {ok, Gamestate}} = thrift_client:call(Client0, 
                                                    new_game, 
                                                    [["Paul", "Sam"], 
                                                     ?scrabbleCheat_GameName_WORDS_WITH_FRIENDS,
                                                     ?scrabbleCheat_Dictionary_ZYNGA]),
    ExWWFDict = ?scrabbleCheat_Dictionary_ZYNGA,
    ExWWFGameName = ?scrabbleCheat_GameName_WORDS_WITH_FRIENDS,
    WWFPairs = [{{1,1}, none},
                {{4,1}, triple_word_score},
                {{3,14}, double_word_score},
                {{7,1},  triple_letter_score},
                {{12,7}, double_word_score}],
    check_game_validity(Gamestate, ExWWFGameName, ExWWFDict, WWFPairs),

    {_Client2, {ok, Lexulous}} = thrift_client:call(Client1,
                                                    new_game, 
                                                    [["Paul", "Sam"], 
                                                     ?scrabbleCheat_GameName_LEXULOUS,
                                                     ?scrabbleCheat_Dictionary_SOWPODS]),
    LGameName = ?scrabbleCheat_GameName_LEXULOUS,
    LDict = ?scrabbleCheat_Dictionary_SOWPODS,
    LPairs = [{{1,1}, triple_word_score},
              {{4,1}, double_letter_score},
              {{3,14}, double_word_score},
              {{7,1}, none},
              {{10,3}, triple_letter_score}],
     check_game_validity(Lexulous, LGameName, LDict, LPairs).


check_game_validity(ThriftGamestate, ExGameName, ExDict, LocBonusPairs) ->
    #gamestate{board = ThriftBoard, game_name = GameName, dict = Dict} = ThriftGamestate,
    Board = thrift_helper:thrift_to_native_board(ThriftBoard),

    ?assert(GameName =:= ExGameName),
    ?assert(Dict =:= ExDict),

    BonusAt = fun(Row,Col) -> tile:get_tile_bonus(board:get_tile(Row, Col, Board)) end,

    lists:foreach(fun({{Row,Col},Bonus}) -> ?assert(BonusAt(Row,Col) =:= Bonus) end, LocBonusPairs).


bad_namelist_test(_Config) ->
    Client0 = get_thrift_client(),
    GameName = ?scrabbleCheat_GameName_SCRABBLE,
    Dict = ?scrabbleCheat_Dictionary_TWL06,

    Thunk1 = fun() -> thrift_client:call(Client0, 
                                         new_game, 
                                         [[], 
                                          GameName,
                                          Dict]) 
             end,
    ?assertException(throw, {_, {exception, {badArgsException, <<"There are no names here!">>}}}, Thunk1()),

    Thunk2 = fun() -> thrift_client:call(Client0, 
                                         new_game, 
                                         [["Sam", "Robert", "PAULSNAMEISTOOLONGLIKETHIS"], 
                                          GameName,
                                          Dict]) 
             end,
    ?assertException(throw, {_, {exception, {badArgsException, _Msg}}}, Thunk2()),

    Thunk3 = fun() -> thrift_client:call(Client0, 
                                         new_game, 
                                         [["Arnegg", "Swartzenolder", [65,4,155]], 
                                          GameName,
                                          Dict]) 
             end,
    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk3()).


bad_dicts_and_gamename_test(_Config) ->
    Client0 = get_thrift_client(),
    GoodDict = ?scrabbleCheat_Dictionary_TWL06,
    GoodName = ?scrabbleCheat_GameName_SCRABBLE,
    Thunk1 = fun() -> thrift_client:call(Client0, new_game, [["Paul", "Sam"], GoodName, 55]) end,
    Thunk2 = fun() -> thrift_client:call(Client0, new_game, [["Paul", "Sam"], 55, GoodDict]) end,
    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk1()),
    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk2()).

    
unallowed_dicts_test(_Config) ->
    Client0 = get_thrift_client(),
    GoodName = ?scrabbleCheat_GameName_WORDS_WITH_FRIENDS,
    BadDict = ?scrabbleCheat_Dictionary_TWL06,  %% WWF only allows the Zynga dictionary
    Thunk1 = fun() -> thrift_client:call(Client0, new_game, [["Paul", "Sam"], GoodName, BadDict]) end,
    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk1()).
    


play_move_test(_Config) ->
    Client0 = get_thrift_client(),
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, 
                                                new_game, 
                                                [["Paul", "Sam"], 
                                                 ?scrabbleCheat_GameName_SCRABBLE,
                                                 ?scrabbleCheat_Dictionary_TWL06]),
    TileList = [tile:new_tile({character, $A}, double_letter_score, 7, 7), 
                tile:new_tile({character, $B}, none, 7, 8), 
                tile:new_tile({character, $L}, double_letter_score, 7, 9), 
                tile:new_tile({character, $E}, none, 7, 10)], 
    ThriftTileList = lists:map(fun thrift_helper:native_to_thrift_tile/1, TileList),
    Score = 8,
    {_Client2, {ok, Gamestate}} = thrift_client:call(Client1, play_move, [ThriftTileList, Fresh]),

    #gamestate{board = Board, scores = Scores, player_turn = CurrentTurn} = Gamestate,
    #gamestate{turn_order = TurnOrder, history = History} = Gamestate,

    NativeBoard = thrift_helper:thrift_to_native_board(Board),
    ?assert(tile:is_occupied(board:get_tile(7,7, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,8, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,9, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,10, NativeBoard))),
    ?assert(not tile:is_occupied(board:get_tile(7,11, NativeBoard))),
    
    ?assert(dict:fetch(<<"Paul">>, Scores) =:= Score),
    ?assert(dict:fetch(<<"Sam">>, Scores) =:= 0),
 
    ?assert(string:equal(CurrentTurn, <<"Sam">>)),
    ?assert(TurnOrder =:= [<<"Paul">>, <<"Sam">>]),

    ?assert(length(History) =:= 1),
    {turn, {move, Move, ReturnedScore}, Player} = hd(History),
    ?assert(lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, ThriftTileList) end, Move)),
    ?assert(<<"Paul">> =:= Player),
    ?assert(ReturnedScore =:= Score).


scrabblecheat_suggestions_test(_Config) ->
    Client0 = get_thrift_client(),
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, 
                                                new_game, 
                                                [["Paul", "Sam"], 
                                                 ?scrabbleCheat_GameName_SCRABBLE,
                                                 ?scrabbleCheat_Dictionary_TWL06]),
    MoveTiles= [tile:new_tile({character, $A}, double_letter_score, 7, 7), 
                tile:new_tile({character, $B}, none, 7, 8), 
                tile:new_tile({character, $L}, double_letter_score, 7, 9), 
                tile:new_tile({character, $E}, none, 7, 10)], 
    ThriftTileList = lists:map(fun thrift_helper:native_to_thrift_tile/1, MoveTiles),
    {Client2, {ok, Gamestate}} = thrift_client:call(Client1, play_move, [ThriftTileList, Fresh]),
    NativeBoard = gamestate:get_gamestate_board(thrift_helper:thrift_to_gamestate(Gamestate)),
    ThriftBoard = thrift_helper:native_to_thrift_board(NativeBoard),
    {_Client3, {ok, Suggestions}} = thrift_client:call(Client2, 
                                                       get_scrabblecheat_suggestions, 
                                                       [<<"TRS">>, 
                                                        ThriftBoard, 
                                                        ?scrabbleCheat_GameName_SCRABBLE,
                                                        ?scrabbleCheat_Dictionary_TWL06]),
    NativeMoves = lists:map(fun ({move, TileList, _Score}) -> 
                                NativeTiles = lists:map(fun thrift_helper:thrift_to_native_tile/1, TileList),
                                Move = lists:foldl(fun move:add_to_move/2, move:new_move(), NativeTiles),
                                Move
                            end, Suggestions),

    Solutions = [{move, [{{character, $S}, none, {7,6}}]}, 
                 {move, [{{character, $R}, none, {7,11}}]},
                 {move, [{{character, $T}, none, {7,6}}]},
                 {move, [{{character, $T}, none, {7,6}},{{character, $S}, none, {7,11}}]},
                 {move, [{{character, $T}, none, {7,6}},{{character, $S}, none, {7,5}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> move:duplicate_moves(X, Y) end, NativeMoves)) end, Solutions).




bad_rack_test(_Config) ->
    Client0 = get_thrift_client(),
    GameName = ?scrabbleCheat_GameName_SCRABBLE,
    Dict = ?scrabbleCheat_Dictionary_TWL06,
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, 
                                                new_game, 
                                                [["Paul", "Sam"], 
                                                 GameName,
                                                 Dict]),
    Dict = ?scrabbleCheat_Dictionary_TWL06,
    MoveTiles= [tile:new_tile({character, $A}, double_letter_score, 7, 7), 
                tile:new_tile({character, $B}, none, 7, 8), 
                tile:new_tile({character, $L}, double_letter_score, 7, 9), 
                tile:new_tile({character, $E}, none, 7, 10)], 
    ThriftTileList = lists:map(fun thrift_helper:native_to_thrift_tile/1, MoveTiles),
    {Client2, {ok, Gamestate}} = thrift_client:call(Client1, play_move, [ThriftTileList, Fresh]),
    NativeBoard = gamestate:get_gamestate_board(thrift_helper:thrift_to_gamestate(Gamestate)),
    ThriftBoard = thrift_helper:native_to_thrift_board(NativeBoard),
    Thunk1 = fun() -> thrift_client:call(Client2, 
                                         get_scrabblecheat_suggestions, 
                                         [<<"">>, 
                                          ThriftBoard, 
                                          GameName,
                                          Dict]) 
             end,
    Thunk2 = fun() -> thrift_client:call(Client2, 
                                         get_scrabblecheat_suggestions, 
                                         [<<"ABCDEFGHIJKLMONPQURSTDKNKLN">>, 
                                          ThriftBoard,
                                          GameName,
                                          Dict]) 
             end,
    Thunk3 = fun() -> thrift_client:call(Client2, 
                                         get_scrabblecheat_suggestions, 
                                         [<<"PAUL&LU">>, 
                                          ThriftBoard,
                                          GameName,
                                          Dict]) 
             end,
    Thunk4 = fun() -> thrift_client:call(Client2, 
                                         get_scrabblecheat_suggestions, 
                                         [<<"lwrcase">>, 
                                          ThriftBoard,
                                          GameName,
                                          Dict]) 
             end,

    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk1()),
    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk2()),
    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk3()),
    ?assertException(throw, {_, {exception, {badArgsException, _Msg2}}}, Thunk4()).



%% bad board
bad_board_test(_Config) ->
    Client1 = get_thrift_client(),
    Rack = <<"ZYGOTE">>,

    % wrong no of tiles
    GI = game_parser:parse_game(scrabble),
    NewCleanBoard = GI#gameinfo.board,

    Thunk1 = fun() ->
                 WrongNumber = board:from_list(lists:map(fun (X) -> tl(X) end, board:as_list(NewCleanBoard))),
                 thrift_helper:native_to_thrift_board(WrongNumber)
             end,
    ?assertException(throw, {badArgsException, _Str}, Thunk1()), 

    Name = ?scrabbleCheat_GameName_SCRABBLE,
    Dict = ?scrabbleCheat_Dictionary_TWL06,

    WordPlacements = [{"ABLE", right, {7,7}}, {"C", down, {6,7}}, {"RE", down, {8,7}}],
    ValidBoard = lists:foldl(fun ({W,D,L},Y) -> board:place_word(W,D,L,Y) end, NewCleanBoard, WordPlacements),

    Board2 = thrift_helper:native_to_thrift_board(board:place_word("ZYGOTE", down, {1,1}, ValidBoard)),

    Board3 = thrift_helper:native_to_thrift_board(board:place_word([257|"PEPS"], right, {11,6}, ValidBoard)),

    Board4 = thrift_helper:native_to_thrift_board(board:place_word("VAGOO", right, {11, 7}, ValidBoard)),

    Board5 = thrift_helper:native_to_thrift_board(board:place_word("VAGOO", down, {6, 11}, ValidBoard)),

    % Islands
    Thunk2 = fun() -> thrift_client:call(Client1, get_scrabblecheat_suggestions, [Rack, Board2, Name, Dict]) end,
    % invalid character on board?
    Thunk3 = fun() -> thrift_client:call(Client1, get_scrabblecheat_suggestions, [Rack, Board3, Name, Dict]) end,
    % Wrong word (Row)
    Thunk4 = fun() -> thrift_client:call(Client1, get_scrabblecheat_suggestions, [Rack, Board4, Name, Dict]) end,
    % Wrong word (Col)
    Thunk5 = fun() -> thrift_client:call(Client1, get_scrabblecheat_suggestions, [Rack, Board5, Name, Dict]) end,

    ?assertException(throw, {_, {exception, {badArgsException, _Msg}}}, Thunk2()),
    ?assertException(throw, {_, {exception, {badArgsException, _Msg}}}, Thunk3()),
    ?assertException(throw, {_, {exception, {badArgsException, _Msg}}}, Thunk4()),
    ?assertException(throw, {_, {exception, {badArgsException, _Msg}}}, Thunk5()).


