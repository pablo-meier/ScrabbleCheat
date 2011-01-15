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

-module(server_test).
-include_lib("eunit/include/eunit.hrl").

-include("scrabbleCheat_thrift.hrl").

-import(main, [start/1, 
               stop/1, 
               new_game/1,
               play_move/2,
               get_scrabblecheat_suggestions/2,
               quit/0]).

-define(PORT, 8888).
-define(LOCALHOST, "127.0.0.1").


%% In most XUnit frameworks, calls to these or related functions are automated... 
%% bleh :(  Maybe a TODO to make these macros eventually, though I don't know much
%% about Erlang macros.
%%
%% In this set of tests, setup and teardown will start new instances of the server,
%% close the server, and create a new client to test calls with.
setup() ->
    Server = main:start(?PORT),
    Client = thrift_client_util:new(?LOCALHOST, ?PORT, scrabbleCheat_thrift, []),
    case Server of 
        {ok, ServerName} -> 
            case Client of 
                {ok, ClientName} -> {ok, ServerName, ClientName};
                _Else -> throw({unexpected_client_value, Client})
            end;
        _Else -> throw({unexpected_server_value, Server})
    end.

teardown(ServerName) ->
    main:stop(ServerName).



new_game_test() ->
    {ok, ServerName, Client0} = setup(),
    {_Client1, {ok, Gamestate}} = thrift_client:call(Client0, new_game, [["Paul", "Sam"]]),
    #gamestate{board = _Board, scores = _Scores, player_turn = CurrentTurn} = Gamestate,
    #gamestate{turn_order = TurnOrder, history = History} = Gamestate,
    ?assert(string:equal(CurrentTurn, <<"Paul">>)),
    ?assert(length(History) =:= 0),
    ?assert(TurnOrder =:= [<<"Paul">>, <<"Sam">>]),
    teardown(ServerName).



play_move_test() ->
    {ok, ServerName, Client0} = setup(),
    {Client1, {ok, Fresh}} = thrift_client:call(Client0, new_game, [["Paul", "Sam"]]),
    TileList = [tile:new_tile({character, $A},double_letter_score,7,7), 
                tile:new_tile({character, $B},none,7,8), 
                tile:new_tile({character, $L},double_letter_score,7,9), 
                tile:new_tile({character, $E},none,7,10)], 
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
    ?assert(Move =:= ThriftTileList),
    ?assert(<<"Paul">> =:= Player),
    ?assert(ReturnedScore =:= Score),
   
    teardown(ServerName).


