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

-module(scrabblecheat_main).

-include("gameinfo.hrl").

-define(DICT_PATH, "priv/dicts/").
-define(DICT_OUT_PATH, "priv/").

-define(WILDCARD, $*).
-define(SMALLEST_ASCII_CHARACTER, 33).
-define(LARGEST_ASCII_CHARACTER, 126).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(PORT, 8888). %% Hard coded for testing, can make this command-line option.


-include("scrabbleCheat_thrift.hrl").

%% APPLICATION API
-export([start_link/0,
        start_link/1,
        stop/1]).
        
%% THRIFT LIBRARY API
-export([handle_function/2]).

%% USER-DEFINED THRIFT API
-export([new_game/3,
         game_info/1,
         pass_turn/1,
         play_move/2,
         get_scrabblecheat_suggestions/4,
         quit/0]).

%% SUPPORT API
-export([get_gameinfo/1,
         make_binary_gaddag/0
        % create_gaddag_looper/0
         ]).



%% make_binary_gaddag :: () -> File ()
%%
%% The program can be invoked to build the data structures and save them disk
%% ahead of time.
make_binary_gaddag() ->
    Paths = lists:map(fun(X) -> 
                          AsStr = atom_to_list(X),
                          {?DICT_PATH ++ AsStr ++ ".txt",
                           ?DICT_OUT_PATH ++ AsStr ++ ".gaddag"}
                      end, [twl06, sowpods, zynga]),

    %% converts priv/dicts/twl06.txt -> priv/twl06.gaddag
    lists:foreach(fun ({In,Out}) -> 
                      dict_parser:output_to_file(In, Out)
                  end, Paths).

%% start_link :: () -> ()
%%
%% Starts a new ScrabbleCheat server on the default port.
start_link() ->
    start_link(?PORT).



%% start :: Int -> ()
%%
%% Starts a new ScrabbleCheat server on the parametrized port. Also sets up 
%% top-level data (reading Gameinfos, making score functions) as necessary.
start_link(Port) ->
    io:format(user, "Scrabblecheat Server starting...~n", []),
    configure_global_data(),
    Handler = ?MODULE,
    thrift_socket_server:start([{handler, Handler},
                                {service, scrabbleCheat_thrift},
                                {port, Port},
                                {socket_opts, [{recv_timeout, 600000}]},
                                {name, scrabbleCheat_server}]).



%% configure_global_data :: () -> ()
%% 
%% Sets up the ETS tables for fetching the various bits of data, such as
%% parsed Gameinfos.
%%
%%       gameinfos -> {game(), GameInfo}
%%
%% where
%%         game() = scrabble | words_with_friends | lexulous
configure_global_data() ->

    GameInfos = lists:map(fun (X) -> {X, game_parser:parse_game(X)} end, 
                          [scrabble, lexulous, words_with_friends]),
    ets:new(gameinfos, [set, protected, named_table, {keypos, 1}]),
    lists:foreach(fun(X) -> ets:insert(gameinfos, X) end, GameInfos).


%% create_gaddag_looper() ->
%%     process_flag(trap_exit, true),
%%     Pid = spawn_link(gaddag_looper, start_link, []),
%%     register(gaddag_looper, Pid),
%%     receive
%%         {'EXIT', Pid, normal} -> ok;
%%         {'EXIT', Pid, shutdown} -> ok;
%%         {'EXIT', Pid, _} ->
%%             create_gaddag_looper()
%%     end.


%% get_gameinfo :: gamename() -> GameInfo
%%
%% Given a Game name, returns the struct of data for that associated game.
get_gameinfo(GameName) ->
    Value = ets:lookup(gameinfos, GameName),
    [{GameName, GameInfo}] = Value,
    GameInfo.


%% EXTERNAL INTERFACE

%% stop :: (or Name Pid) -> ()
%%
%% Stops the server named by the parameter, or its Pid.
stop(Server) ->
    ets:delete(gameinfos),
    thrift_socket_server:stop(Server).

%% THRIFT INTERFACE 
handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.

debug(Format, Data) ->
    io:format(user, Format, Data).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  THRIFT DEFINITION INTERFACE


%% new_game :: [String] -> Gamestate
%%
%% Given a list of players, return a fresh gamestate to start a new game.  
%% Throws BadArgsException if the list is empty, or the player's names are
%% too long.
new_game(Playerlist, ThriftGameName, ThriftDict) ->
    debug("New Game for ~p~n", [Playerlist]),
    Stringlist = lists:map(fun binary_to_list/1, Playerlist),
    validate_namelist(Stringlist),
    try
        Game = thrift_helper:as_native_game(ThriftGameName),
        Dict = thrift_helper:as_native_dict(ThriftDict),
        validate_pairing(Game, Dict),
        Gamestate = gamestate:fresh_gamestate(Stringlist, Game, Dict),
        thrift_helper:gamestate_to_thrift(Gamestate)
    catch
        throw:{not_valid_thrift_game, _} -> throw({badArgsException, "Game name in Gamestate is invalid."});
        throw:{not_valid_thrift_dictionary, _} -> throw({badArgsException, "Dictionary in Gamestate invalid."})
    end.


%% game_info :: gamename() -> GameInfo
%%
%% where gamename() :: scrabble | words_with_friends | lexulous
%%
%% A client may want to receive information about a game, such as the letter 
%% distribution, or what its blank board looks like.  We give that data back 
%% as a structure.
game_info(ThriftName) ->
    debug("Game_info for ~p~n", [ThriftName]),
    try
        GameName = thrift_helper:as_native_game(ThriftName),
        Gameinfo = game_parser:parse_game(GameName),
        thrift_helper:gameinfo_to_thrift(Gameinfo)
    catch
        throw:{not_valid_thrift_game, _} -> throw({badArgsException, "Game name is invalid."})
    end.


%% pass_turn :: Gamestate -> Gamestate
%%
%% Allows a Player to pass their turn without playing a move.  Advances the 
%% current turn forward.
pass_turn(Gamestate) ->
    NativeGamestate = thrift_helper:thrift_to_gamestate(Gamestate),
    verify_gamestate(NativeGamestate),
    WithTurnPassed = gamestate:pass_turn(NativeGamestate),
    thrift_helper:gamestate_to_thrift(WithTurnPassed).


%% play_move :: [ThriftTile] * ThriftGamestate -> ThriftGamestate
%%
%% Given a set of tiles and a gamestate, returns a new gamestate with the tiles 
%% placed as a move.  Throws BadMoveException, or BadGamestateException if incoming 
%% data is invalid.
play_move(ThriftTiles, ThriftGamestate) ->
    io:format(user, "play_move for ~p tiles~n", [ThriftTiles]),
    try
        Tiles = lists:map(fun thrift_helper:thrift_to_native_tile/1, ThriftTiles),
        Gamestate = thrift_helper:thrift_to_gamestate(ThriftGamestate),
        io:format(user, "calling verify_gamestate", []),
        case verify_gamestate(Gamestate) of
            gamestate_ok -> ok;
            {error, Reason} -> io:format(user, "play_move failed: ~p~n", [Reason]);
            Other -> io:format(user, "unexpected response: ~p~n", [Other]) 
        end,
    
        Move = move:from_list(Tiles),
        case verify_move(Move, Gamestate) of
            move_ok -> ok;
            {error, Reason2} -> io:format(user, "play_move failed: ~p~n", [Reason2]);
            Other2 -> io:format(user, "unexpected response: ~p~n", [Other2]) 
        end,
        WithMove = gamestate:play_move(Gamestate, Move),

        case verify_gamestate(WithMove) of
            gamestate_ok -> ok;
            {error, Reason3} -> io:format(user, "play_move failed: ~p~n", [Reason3]);
            Other3 -> io:format(user, "unexpected response: ~p~n", [Other3]) 
        end,
        thrift_helper:gamestate_to_thrift(WithMove)
    catch
        throw:{not_valid_thrift_game, _} -> throw({badArgs, "Game name in Gamestate is invalid."});
        throw:{not_valid_thrift_dictionary, _} -> throw({badArgs, "Dictionary in Gamestate invalid."})
    end.


%% get_scrabblecheat_suggestions :: String * ThriftBoard -> [ThriftMove]
%%
%% Given a rack and a board, return a list of Thrift-compliant moves that 
%% clients can use.  Throws BadArgsException and BadBoardException, if your
%% incoming data sucks.
get_scrabblecheat_suggestions(Rack, Board, ThriftName, ThriftDict) ->
    debug("get_scrabblecheat_suggestions for rack ~p~n", [Rack]),
    GameName = thrift_helper:as_native_game(ThriftName),
    Dict = thrift_helper:as_native_dict(ThriftDict),

    RackAsString = binary_to_list(Rack),
    validate_rack(RackAsString, GameName),

    NativeBoard = thrift_helper:thrift_to_native_board(Board),
    verify_board(NativeBoard, Dict),

    Moves = search_for_moves(NativeBoard, RackAsString, Dict),

    WithScores = lists:map(fun (X) -> {X, move:score(X, NativeBoard, GameName)} end, Moves),
    Sorted = lists:reverse(lists:keysort(2, WithScores)),
    lists:map(fun ({NativeMove, Score}) ->
                  Tiles = move:get_move_tiles(NativeMove),
                  ThriftTiles = lists:map(fun thrift_helper:native_to_thrift_tile/1, Tiles),
                  {move, ThriftTiles, Score}
              end, Sorted).


%% quit :: () -> ()
%%
%% Receive the quit notification from a client.  Should really take in a Pid or something.
quit() ->
    debug("Quit message received.~n", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  SYNCHRONOUS CALLS TO GEN_SERVER 


verify_gamestate(GS) ->
    gen_server:call(gaddag_looper, {verify_gamestate, GS}).

verify_move(Move, Gamestate) ->
    gen_server:call(gaddag_looper, {verify_move, Move, Gamestate}).

verify_board(Board, Dict) ->
    gen_server:call(gaddag_looper, {verify_board, Board, Dict}).

search_for_moves(Board, Rack, Dict) ->
    gen_server:call(gaddag_looper, {search_for_moves, Board, Rack, Dict}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  HELPERS
validate_rack([], _) -> throw({badArgsException, "This rack is empty!"});
validate_rack(Rack, GameName) ->
    GameInfo = game_parser:parse_game(GameName),
    MaxLength = GameInfo#gameinfo.racksize,
    Len = (length(Rack) =< MaxLength),
    Valid = lists:all(fun (X) -> (X >= $A andalso X =< $Z) orelse X =:= ?WILDCARD end, Rack),
    case {Len, Valid} of
        {true, true} -> ok;
        {false, _} -> throw({badArgsException, "Rack is too long!"});
        {_, false} -> throw({badArgsException, "There is an invalid character in your rack."})
    end.


%% validate_namelist :: [String] -> ()
%%
%% Checks the validity of the list of names.  If the list is empty or the names aren't
%% well formed, we throw an exception.
validate_namelist([]) -> throw(#badArgsException{reprimand="There are no names here!"});
validate_namelist(Lst) ->
    case is_list(Lst) of
        false -> throw(#badArgsException{reprimand="This is not a list!"});
        true ->
                lists:foreach(fun validate_length/1,  Lst),
                lists:foreach(fun (X) -> lists:foreach(fun validate_list_contents/1, X) end, Lst),
                ok
    end.

validate_list_contents(Elem) ->
    case is_integer(Elem) of
        false -> 
            Reprimand = lists:concat(["Element \"", [Elem], "\" not valid in a name."]),
            throw(#badArgsException{reprimand=Reprimand});
        true -> 
            if 
                Elem < ?SMALLEST_ASCII_CHARACTER orelse Elem > ?LARGEST_ASCII_CHARACTER ->
                    Reprimand = lists:concat(["Character \"", [Elem], "\" not valid in a name."]),
                    throw(#badArgsException{reprimand=Reprimand});
                true -> ok
            end
    end.

validate_length(Name) ->
    if 
        length(Name) > 15 -> 
            Message = lists:concat(["The name ", Name, " is too long!  15 Characters or less, please ^_^"]),
            throw(#badArgsException{reprimand=Message});
        true -> ok
    end.


%% validate_pairing :: gamename() * dict() -> ok | EXCEPTION
%%
%% We throw a badargs exception of the player is trying to perform an operation
%% with a disallowed pairing of game and dictionary, e.g. words_with_friends on
%% twl06.
validate_pairing(GameName, Dict) ->
    Gameinfo = get_gameinfo(GameName),
    AllowedDicts = Gameinfo#gameinfo.allowed_dicts,
    Allowed = lists:any(fun (X) -> X =:= Dict end, AllowedDicts),
    case Allowed of
        true -> ok;
        false -> 
            throw({badArgsException, "Invalid Dictionary for the game you are playing."}) 
     end.


