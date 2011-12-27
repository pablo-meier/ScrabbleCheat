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


-define(check_for_exception(Thunk), ?assertException(throw, {_, {exception, {badArgsException, _}}}, Thunk())).

%% Test server callbacks.
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         suite/0]).

%% Test cases
-export([new_game_test/1,
         gameinfo_test/1,
         play_move_scrabble_test/1,
         play_move_lexulous_test/1,
         play_move_wwf_test/1,
         play_move_bad_args/1,
         scrabblecheat_suggestions_test/1,
         annalisa_test/1,
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
     gameinfo_test,
     play_move_scrabble_test,
     play_move_lexulous_test,
     play_move_bad_args,
%%     play_move_wwf_test, %% Test disabled because of memory bullshit.
     scrabblecheat_suggestions_test,
     annalisa_test,
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
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              NEW GAME TESTS                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
                {{3,14}, double_letter_score},
                {{7,1},  triple_letter_score},
                {{12,8}, double_word_score}],
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
    ?check_for_exception(Thunk2),

    Thunk3 = fun() -> thrift_client:call(Client0, 
                                         new_game, 
                                         [["Arnegg", "Swartzenolder", [65,4,155]], 
                                          GameName,
                                          Dict]) 
             end,
    ?check_for_exception(Thunk3).


bad_dicts_and_gamename_test(_Config) ->
    Client0 = get_thrift_client(),
    GoodDict = ?scrabbleCheat_Dictionary_TWL06,
    GoodName = ?scrabbleCheat_GameName_SCRABBLE,
    Thunk1 = fun() -> thrift_client:call(Client0, new_game, [["Paul", "Sam"], GoodName, 55]) end,
    Thunk2 = fun() -> thrift_client:call(Client0, new_game, [["Paul", "Sam"], 55, GoodDict]) end,
    ?check_for_exception(Thunk1),
    ?check_for_exception(Thunk2).

    
unallowed_dicts_test(_Config) ->
    Client0 = get_thrift_client(),
    GoodName = ?scrabbleCheat_GameName_WORDS_WITH_FRIENDS,
    BadDict = ?scrabbleCheat_Dictionary_TWL06,  %% WWF only allows the Zynga dictionary
    Thunk1 = fun() -> thrift_client:call(Client0, new_game, [["Paul", "Sam"], GoodName, BadDict]) end,
    ?check_for_exception(Thunk1).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              GAMEINFO TESTS                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


gameinfo_test(_Config) ->
    ClientScrabble = get_thrift_client(),
    ClientWWF = get_thrift_client(),
    ClientLexulous = get_thrift_client(),
    ClientBad = get_thrift_client(),

    {_, {ok, ThriftGameInfoS}} = thrift_client:call(ClientScrabble, 
                                                    game_info, 
                                                    [?scrabbleCheat_GameName_SCRABBLE]),
    ScrabbleBoard = (game_parser:parse_game(scrabble))#gameinfo.board,
    PropListS = [{game_name, ?scrabbleCheat_GameName_SCRABBLE},
                 {rack_size, 7},
                 {bingo_bonus, [{7, 50}]},
                 {letter_distribution, [{<<"E">>,12},{<<"O">>,8},{<<"F">>, 2}]},
                 {score_distribution,  [{<<"K">>,5}, {<<"B">>,3},{<<"H">>,4}]},
                 {allowed_dictionaries, [?scrabbleCheat_Dictionary_TWL06,
                                         ?scrabbleCheat_Dictionary_SOWPODS]},
                 {board_template, ScrabbleBoard}],
    test_gameinfo(ThriftGameInfoS, PropListS),

    {_, {ok, ThriftGameInfoL}} = thrift_client:call(ClientLexulous, 
                                                    game_info, 
                                                    [?scrabbleCheat_GameName_LEXULOUS]),
    LexulousBoard = (game_parser:parse_game(lexulous))#gameinfo.board,
    PropListL = [{game_name, ?scrabbleCheat_GameName_LEXULOUS},
                 {rack_size, 8},
                 {bingo_bonus, [{7,40},{8,50}]},
                 {letter_distribution, [{<<"E">>,12},{<<"O">>,8},{<<"F">>,2}]},
                 {score_distribution,  [{<<"K">>,6}, {<<"B">>,4},{<<"H">>,5}]},
                 {allowed_dictionaries, [?scrabbleCheat_Dictionary_TWL06,
                                         ?scrabbleCheat_Dictionary_SOWPODS]},
                 {board_template, LexulousBoard}],
    test_gameinfo(ThriftGameInfoL, PropListL),

    {_, {ok, ThriftGameInfoWWF}} = thrift_client:call(ClientWWF, 
                                                      game_info, 
                                                      [?scrabbleCheat_GameName_WORDS_WITH_FRIENDS]),
    WWFBoard = (game_parser:parse_game(words_with_friends))#gameinfo.board,
    PropListWWF = [{game_name, ?scrabbleCheat_GameName_WORDS_WITH_FRIENDS},
                   {rack_size, 7},
                   {bingo_bonus, [{7,35}]},
                   {letter_distribution, [{<<"E">>,13},{<<"O">>,8},{<<"F">>,2}]},
                   {score_distribution,  [{<<"K">>,5}, {<<"B">>,4},{<<"H">>,3}]},
                   {allowed_dictionaries, [?scrabbleCheat_Dictionary_ZYNGA]},
                   {board_template, WWFBoard}],
    test_gameinfo(ThriftGameInfoWWF, PropListWWF),

    BadThunk = fun() -> thrift_client:call(ClientBad, game_info, [55]) end,
    ?check_for_exception(BadThunk).


test_gameinfo(ThriftGameInfo, PropList) ->
    GameName = proplists:get_value(game_name, PropList),

    %% Erlang is throwing a badrecord exception, and I don't have the patience to figure it
    %% out properly.
%    {gameInfo, Name, RackSize, ActualBingos, ActualLetterDist,
%               ActualScoreDist, ActualDicts, ActualBoard}= ThriftGameInfo,

    ?assert(ThriftGameInfo#gameInfo.name =:= GameName),

    RackSize = proplists:get_value(rack_size, PropList),
    ?assert(ThriftGameInfo#gameInfo.rack_size =:= RackSize),
    
    ExpectedBingos = proplists:get_value(bingo_bonus, PropList),
    ActualBingos = ThriftGameInfo#gameInfo.bingo_bonus,
    lists:foreach(fun({X,Y}) -> ?assert(dict:fetch(X, ActualBingos) =:= Y) end, ExpectedBingos),

    ExpectedLetterDist = proplists:get_value(letter_distribution, PropList),
    ActualLetterDist = ThriftGameInfo#gameInfo.letter_distribution,
    lists:foreach(fun({X,Y}) -> ?assert(dict:fetch(X, ActualLetterDist) =:= Y) end, ExpectedLetterDist),

    ExpectedScoreDist = proplists:get_value(score_distribution, PropList),
    ActualScoreDist = ThriftGameInfo#gameInfo.score_distribution,
    lists:foreach(fun({X,Y}) -> ?assert(dict:fetch(X, ActualScoreDist) =:= Y) end, ExpectedScoreDist),

%    ExpectedDicts = proplists:get_value(allowed_dictionaries, PropList),
%    ActualDicts = ThriftGameInfo#gameInfo.allowed_dictionaries,
%    lists:all(fun (X) -> 
%                  io:format(user, "Entering Dictionary comparison for ~p~n...", [X]),
%                  Compare = lists:any(fun (Y) -> 
%                                        io:format(user, "   comparing against ~p~n...", [Y]),
%                                        X =:= Y 
%                                    end, ExpectedDicts),
%                  io:format(user, "Final value is ~p~n", [Compare]),
%                  ?assert(Compare)
%              end, ActualDicts),

    ExpectedBoard = proplists:get_value(board_template, PropList),
    ActualBoard = thrift_helper:thrift_to_native_board(ThriftGameInfo#gameInfo.board_template),
    ?assert(ActualBoard =:= ExpectedBoard).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             PLAY MOVE TESTS                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_move_scrabble_test(_Config) ->
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
    {Client2, {ok, Gamestate}} = thrift_client:call(Client1, play_move, [ThriftTileList, Fresh]),

    #gamestate{board = Board, scores = Scores, player_turn = CurrentTurn} = Gamestate,
    #gamestate{turn_order = TurnOrder, history = History} = Gamestate,

    %% Assert that the tiles are present.
    NativeBoard = thrift_helper:thrift_to_native_board(Board),
    ?assert(tile:is_occupied(board:get_tile(7,7, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,8, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,9, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,10, NativeBoard))),
    ?assert(not tile:is_occupied(board:get_tile(7,11, NativeBoard))),
    
    %% Assert the scoring is correct.
    ?assert(dict:fetch(<<"Paul">>, Scores) =:= Score),
    ?assert(dict:fetch(<<"Sam">>, Scores) =:= 0),
 
    %% Assert that the turn has moved.
    ?assert(string:equal(CurrentTurn, <<"Sam">>)),
    ?assert(TurnOrder =:= [<<"Paul">>, <<"Sam">>]),

    %% Assert that the history has been augmented.
    ?assert(length(History) =:= 1),
    {turn, {move, Move, ReturnedScore}, Player} = hd(History),
    ?assert(lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, ThriftTileList) end, Move)),
    ?assert(<<"Paul">> =:= Player),
    ?assert(ReturnedScore =:= Score),
    
    %% Now do it again, with a Bingo!

    BingoTiles = [tile:new_tile({character, $H}, triple_letter_score, 2, 10), 
                  tile:new_tile({character, $A}, none, 3, 10), 
                  tile:new_tile({character, $R}, none, 4, 10), 
                  tile:new_tile({character, $D}, none, 5, 10), 
                  tile:new_tile({character, $N}, triple_letter_score, 6, 10), 
                  tile:new_tile({character, $S}, none, 8, 10), 
                  tile:new_tile({character, $S}, none, 9, 10)], 
    BingoMoveList = lists:map(fun thrift_helper:native_to_thrift_tile/1, BingoTiles),
    {_Client3, {ok, WithBingo}} = thrift_client:call(Client2, play_move, [BingoMoveList, Gamestate]),
    BingoScore =  72, % 50 + 12 + 1 + 1 + 2 + 3 + 1 + 1 + 1

    #gamestate{scores = BingoScores, player_turn = BingoTurn} = WithBingo,

    %% Assert the scoring is correct.
    Result = dict:fetch(<<"Sam">>, BingoScores),
    ?assert( Result =:= BingoScore),
    ?assert(BingoTurn =:= <<"Paul">>).


play_move_lexulous_test(_Config) ->
    Client0 = get_thrift_client(),
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, 
                                                new_game, 
                                                [["Paul", "Sam"], 
                                                 ?scrabbleCheat_GameName_LEXULOUS,
                                                 ?scrabbleCheat_Dictionary_TWL06]),
    TileList = [tile:new_tile({character, $H}, none, 7, 7),
                tile:new_tile({character, $O}, none, 7, 8),
                tile:new_tile({character, $L}, none, 7, 9),
                tile:new_tile({character, $E}, double_letter_score, 7, 10)],
    ThriftTileList = lists:map(fun thrift_helper:native_to_thrift_tile/1, TileList),
    Score = 9,
    {Client2, {ok, Gamestate}} = thrift_client:call(Client1, play_move, [ThriftTileList, Fresh]),

    #gamestate{board = Board, scores = Scores, player_turn = CurrentTurn} = Gamestate,
    #gamestate{turn_order = TurnOrder, history = History} = Gamestate,

    %% Assert that the tiles are present.
    NativeBoard = thrift_helper:thrift_to_native_board(Board),
    ?assert(tile:is_occupied(board:get_tile(7,7, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,8, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,9, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,10, NativeBoard))),
    ?assert(not tile:is_occupied(board:get_tile(7,11, NativeBoard))),
    
    %% Assert the scoring is correct.
    ?assert(dict:fetch(<<"Paul">>, Scores) =:= Score),
    ?assert(dict:fetch(<<"Sam">>, Scores) =:= 0),
 
    %% Assert that the turn has moved.
    ?assert(string:equal(CurrentTurn, <<"Sam">>)),
    ?assert(TurnOrder =:= [<<"Paul">>, <<"Sam">>]),

    %% Assert that the history has been augmented.
    ?assert(length(History) =:= 1),
    {turn, {move, Move, ReturnedScore}, Player} = hd(History),
    ?assert(lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, ThriftTileList) end, Move)),
    ?assert(<<"Paul">> =:= Player),
    ?assert(ReturnedScore =:= Score),
    
    %% Now do it again, with a Bingo!.
    BingoTiles = [tile:new_tile({character, $H}, none, 2, 10), 
                  tile:new_tile({character, $A}, triple_letter_score, 3, 10), 
                  tile:new_tile({character, $R}, none, 4, 10), 
                  tile:new_tile({character, $D}, none, 5, 10), 
                  tile:new_tile({character, $N}, none, 6, 10), 
                  tile:new_tile({character, $S}, none, 8, 10), 
                  tile:new_tile({character, $S}, double_letter_score, 9, 10)], 
    BingoMoveList = lists:map(fun thrift_helper:native_to_thrift_tile/1, BingoTiles),
    {Client3, {ok, WithBingo}} = thrift_client:call(Client2, play_move, [BingoMoveList, Gamestate]),
    BingoScore =  56, % 40 + 5 + 3 + 1 + 2 + 1 + 1 + 1 + 2

    #gamestate{scores = BingoScores, player_turn = BingoTurn} = WithBingo,

    %% Assert the scoring is correct.
    Result = dict:fetch(<<"Sam">>, BingoScores),
    ?assert(Result =:= BingoScore),
    ?assert(BingoTurn =:= <<"Paul">>),
    
    With8Tiles = [tile:new_tile({character, $A}, none, 3, 7), 
                  tile:new_tile({character, $B}, none, 4, 7), 
                  tile:new_tile({character, $A}, none, 5, 7), 
                  tile:new_tile({character, $S}, double_letter_score, 6, 7), 
                  tile:new_tile({character, $E}, none, 8, 7), 
                  tile:new_tile({character, $D}, none, 9, 7), 
                  tile:new_tile({character, $L}, double_letter_score, 10, 7), 
                  tile:new_tile({character, $Y}, none, 11, 7)], 
    With8MoveList = lists:map(fun thrift_helper:native_to_thrift_tile/1, With8Tiles),
    {Client3, {ok, With8}} = thrift_client:call(Client3, play_move, [With8MoveList, WithBingo]),
    With8Score =  73, % 50 + 1 + 4 + 1 + 2 + H + 1 + 2 + 2 + 5

    #gamestate{scores = With8Scores, player_turn = With8Turn} = With8,

    %% Assert the scoring is correct.
    ResultWith8 = dict:fetch(<<"Paul">>, With8Scores),
    ?assert(ResultWith8 =:= With8Score + Score),
    ?assert(With8Turn =:= <<"Sam">>).



play_move_wwf_test(_Config) ->
    Client0 = get_thrift_client(),
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, 
                                                new_game, 
                                                [["Paul", "Sam"], 
                                                 ?scrabbleCheat_GameName_WORDS_WITH_FRIENDS,
                                                 ?scrabbleCheat_Dictionary_ZYNGA]),
    TileList = [tile:new_tile({character, $H}, none, 7, 7), 
                tile:new_tile({character, $O}, none, 7, 8), 
                tile:new_tile({character, $L}, none, 7, 9), 
                tile:new_tile({character, $E}, none, 7, 10)], 
    ThriftTileList = lists:map(fun thrift_helper:native_to_thrift_tile/1, TileList),
    Score = 7,
    {_Client2, {ok, Gamestate}} = thrift_client:call(Client1, play_move, [ThriftTileList, Fresh]),

    #gamestate{board = Board, scores = Scores, player_turn = CurrentTurn} = Gamestate,
    #gamestate{turn_order = TurnOrder, history = History} = Gamestate,

    %% Assert that the tiles are present.
    NativeBoard = thrift_helper:thrift_to_native_board(Board),
    ?assert(tile:is_occupied(board:get_tile(7,7, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,8, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,9, NativeBoard))),
    ?assert(tile:is_occupied(board:get_tile(7,10, NativeBoard))),
    ?assert(not tile:is_occupied(board:get_tile(7,11, NativeBoard))),
    
    %% Assert the scoring is correct.
    ?assert(dict:fetch(<<"Paul">>, Scores) =:= Score),
    ?assert(dict:fetch(<<"Sam">>, Scores) =:= 0),
 
    %% Assert that the turn has moved.
    ?assert(string:equal(CurrentTurn, <<"Sam">>)),
    ?assert(TurnOrder =:= [<<"Paul">>, <<"Sam">>]),

    %% Assert that the history has been augmented.
    ?assert(length(History) =:= 1),
    {turn, {move, Move, ReturnedScore}, Player} = hd(History),
    ?assert(lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, ThriftTileList) end, Move)),
    ?assert(<<"Paul">> =:= Player),
    ?assert(ReturnedScore =:= Score)
    
    %% Now do it again, with a Bingo!.
    .


%% Should test that 
%%   - a move that doesn't match the board is rejected.
%%   - a move that is an island is rejected.
%%   - a bad gamestate is rejected.
%%   - an incorrect word IS NOT, by default, rejected. Need to add the optional
%%     bit later.
play_move_bad_args(_Config) ->
    Client0 = get_thrift_client(),
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, 
                                                new_game, 
                                                [["Paul", "Sam"], 
                                                 ?scrabbleCheat_GameName_SCRABBLE,
                                                 ?scrabbleCheat_Dictionary_TWL06]),
    TileList = [tile:new_tile({character, $A}, triple_word_score, 7, 7), 
                tile:new_tile({character, $B}, triple_word_score, 7, 8), 
                tile:new_tile({character, $L}, triple_word_score, 7, 9), 
                tile:new_tile({character, $E}, triple_word_score, 7, 10)], 
    ThriftTileList = lists:map(fun thrift_helper:native_to_thrift_tile/1, TileList),
    BadThunk = fun () -> thrift_client:call(Client1, play_move, [ThriftTileList, Fresh]) end,
    ?check_for_exception(BadThunk),
    
    GoodList = [tile:new_tile({character, $A}, double_letter_score, 7, 7), 
                tile:new_tile({character, $B}, none, 7, 8), 
                tile:new_tile({character, $L}, double_letter_score, 7, 9), 
                tile:new_tile({character, $E}, none, 7, 10)], 
    TL = lists:map(fun thrift_helper:native_to_thrift_tile/1, GoodList),
    {Client2, {ok, WithMove}} = thrift_client:call(Client1, play_move, [TL, Fresh]),
    Island = [tile:new_tile({character, $C}, none, 5, 7), 
              tile:new_tile({character, $A}, none, 5, 8), 
              tile:new_tile({character, $R}, none, 5, 9), 
              tile:new_tile({character, $E}, none, 5, 10)],
    IL = lists:map(fun thrift_helper:native_to_thrift_tile/1, Island),
    IslandThunk = fun () -> thrift_client:call(Client1, play_move, [IL, WithMove]) end,
    ?check_for_exception(IslandThunk),

    %% Assert that a bullshit move passes.
    BSMove = [tile:new_tile({character, $X}, none, 6, 7), 
              tile:new_tile({character, $X}, none, 6, 8), 
              tile:new_tile({character, $R}, none, 6, 9), 
              tile:new_tile({character, $E}, triple_letter_score, 6, 10)],
    BSL = lists:map(fun thrift_helper:native_to_thrift_tile/1, BSMove),
    _DontCrash = thrift_client:call(Client2, play_move, [BSL, WithMove]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      SCRABBLECHEAT SUGGESTIONS TEST                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%% Make sure the server throws an exception if you feed it a bad board. These were 
%% taken when I tried to use the bot in an ad-hoc way, and fed it the wrong parameters. Hilarity (read: pain)
%% ensued.
annalisa_test(_Config) -> 
    Client0 = get_thrift_client(),
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, 
                                                new_game, 
                                                [["Paul", "Sam"], 
                                                 ?scrabbleCheat_GameName_SCRABBLE,
                                                 ?scrabbleCheat_Dictionary_TWL06]),

    Tiles = [{6, 10, $C},
            {7, 10, $A},
            {8, 10, $T},

            {7, 9, $F},
            {7, 11, $X},

            {8, 8, $B},
            {8, 9, $I},

            {9, 8, $R},
            {10, 8, $E},
            {11, 8, $N},
            {12, 8, $T},

            {8, 12, $G},
            {9, 12, $A},
            {10, 12, $L},
            {11, 12, $E},
            {12, 12, $S},

            {10, 7, $J},
            {10, 9, $A},
            {10, 10, $N},

            {11, 6, $S},
            {11, 7, $O},

            {12, 7, $T},
            {13, 7, $A},
            {14, 7, $Y},

            {13, 5, $K},

            {13, 6, $A},

            {12, 9, $U},
            {12, 10, $B},
            {12, 11, $E},

            {13, 10, $U},
            {14, 10, $M},

            {14, 11, $U},
            {14, 12, $S},
            {14, 13, $E}
           ],
	BlankBoard = thrift_helper:thrift_to_native_board(Fresh#gamestate.board),
    NativeBoard = lists:foldl(fun ({A,B,C}, Accum) -> tile_template(A,B,C,Accum) end, BlankBoard, Tiles),
    ThriftBoard = thrift_helper:native_to_thrift_board(NativeBoard),
    BadThunk = fun () -> thrift_client:call(Client1, 
                                             get_scrabblecheat_suggestions, 
                                             [<<"OOIAIIT">>, 
                                             ThriftBoard, 
                                             ?scrabbleCheat_GameName_SCRABBLE,
                                             ?scrabbleCheat_Dictionary_TWL06]) end,
    ?check_for_exception(BadThunk).

tile_template(Row, Col, Char, Board) ->
	board:place_letter_on_board(Row, Col, Char, Board, false).

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

    ?check_for_exception(Thunk1),
    ?check_for_exception(Thunk2),
    ?check_for_exception(Thunk3),
    ?check_for_exception(Thunk4).



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
    %% Testing outside of the thrift_server (we're calling a board function here) requires a different
    %% format than check_for_exception allows.
    ?assertException(throw, {badArgsException, _Msg}, Thunk1()), 

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

    ?check_for_exception(Thunk2),
    ?check_for_exception(Thunk3),
    ?check_for_exception(Thunk4),
    ?check_for_exception(Thunk5).


