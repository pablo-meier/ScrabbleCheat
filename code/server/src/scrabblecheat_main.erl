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

-import(movesearch, [get_best_move_function/1]).
-import(board, [print_board/1, place_move_on_board/2]).
-import(string_utils, [format_string_for_gaddag/1]).
-import(move, [score/2]).
-import(lists, [reverse/1,foreach/2, keysort/2, sort/2, map/2]).

-include("gameinfo.hrl").

-define(DICT_FILE, "../test/testdict.txt").
-define(LARGE_DICT_FILE, "priv/dicts/twl06.txt").
-define(DICT_BIN_PATH, "priv/twl06.gaddag").
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
-export([get_master_gaddag/1,
         make_binary_gaddag/0]).



%% make_binary_gaddag :: () -> File ()
%%
%% The program can be invoked to build the data structures and save them disk
%% ahead of time.
make_binary_gaddag() ->
    dict_parser:output_to_file(?LARGE_DICT_FILE, ?DICT_BIN_PATH).




%% start :: () -> ()
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
%% parsed Gameinfos, score functions, and GADDAGs.  We do this with three
%% ETS tables:  
%%
%%         gaddags -> {dictionary(), Gaddag}
%%  word_functions -> {dictionary(), (Rack * Board -> [Move])}
%%       gameinfos -> {game(), GameInfo}
%% score_functions -> {game(), (Move * Board -> Int)}
%%
%% where
%%   dictionary() = twl06 | sowpods | zynga
%%         game() = scrabble | words_with_friends | lexulous
configure_global_data() ->

    Gaddag = get_or_make_gaddag(),
    make_named_table(gaddags),
    ets:insert(gaddags, {twl06, Gaddag}),

    WordFunction = get_best_move_function(Gaddag),
    make_named_table(word_functions),
    ets:insert(word_functions, {twl06, WordFunction}),
    
    GameInfos = lists:map(fun (X) -> {X, game_parser:parse_game(X)} end, 
                          [scrabble, lexulous, words_with_friends]),
    make_named_table(gameinfos),
    lists:foreach(fun(X) -> ets:insert(gameinfos, X) end, GameInfos),
    
    ScoreFuns = lists:map(fun({X,Y}) -> {X, move:make_score_function(Y)} end, GameInfos),
    make_named_table(score_functions),
    lists:foreach(fun(X) -> ets:insert(score_functions, X) end, ScoreFuns).


make_named_table(Name) ->
    ets:new(Name, [set, protected, named_table, {keypos, 1}]).


%% get_or_make_gaddag :: () -> Gaddag
%%
%% Searches a few predefined paths for files to produce a dictionary Gaddag at
%% boot time.  Either finds a predefined binary one, or produces one.
get_or_make_gaddag() ->
    PrivDir = code:priv_dir(scrabblecheat),
    Path = string:concat(PrivDir, "/twl06.gaddag"),
    case file:read_file_info(Path) of
        {ok, _} -> 
            io:format("Reading a dictionary, this may take a few seconds...~n"),
            dict_parser:read_from_binary(Path);
        {error, _} -> 
            io:format("Gaddag file not found!  Using test dictionary in meantime.~n"),
            io:format("run `make binary-gaddag` to generate the full dictionary.~n"),
            case file:read_file_info(?DICT_FILE) of
                {ok, _} -> dict_parser:parse(?DICT_FILE);
                {error, _} -> dict_parser:parse("test/testdict.txt")
            end
        end.


%% get_search_function :: dictionary() -> (Board * Rack -> [Move])
%%
%% Returns a search function that takes a board and rack, and produces
%% a list of moves.
get_search_function(Dict) ->
    [{Dict, Search}] = ets:lookup(word_functions, Dict),
    Search.


%% get_search_function :: dictionary() -> (Board * Rack -> [Move])
%%
%% Returns a search function that takes a board and rack, and produces
%% a list of moves.
get_score_function(Game) ->
    [{Game, Fun}] = ets:lookup(score_functions, Game),
    Fun.


%% get_master_gaddag :: dictionary() -> Gaddag
%%
%% Get's a master, top-level Gaddag.  These are mostly used for verification
%% of moves and boards.
get_master_gaddag(Dict) ->
    [{Dict, Gaddag}] = ets:lookup(gaddags, Dict),
    Gaddag.


%% get_gameinfo :: gamename() -> GameInfo
%%
%% Given a Game name, returns the struct of data for that associated game.
get_gameinfo(GameName) ->
    [{GameName, GameInfo}] = ets:lookup(gameinfos, GameName),
    GameInfo.


%% EXTERNAL INTERFACE


%% stop :: (or Name Pid) -> ()
%%
%% Stops the server named by the parameter, or its Pid.
stop(Server) ->
    ets:delete(gaddags),
    ets:delete(word_functions),
    ets:delete(score_functions),
    ets:delete(gameinfos),
    thrift_socket_server:stop(Server).

%% THRIFT INTERFACE 
handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).


%% new_game :: [String] -> Gamestate
%%
%% Given a list of players, return a fresh gamestate to start a new game.  
%% Throws BadArgsException if the list is empty, or the player's names are
%% too long.
new_game(Playerlist, ThriftGameName, ThriftDict) ->
    debug("New Game for ~p~n", Playerlist),
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



%% game_info :: gamename() -> GameInfo
%%
%% where gamename() :: scrabble | words_with_friends | lexulous
%%
%% A client may want to receive information about a game, such as the letter 
%% distribution, or what its blank board looks like.  We give that data back 
%% as a structure.
game_info(ThriftName) ->
    debug("Game_info for ~p~n", ThriftName),
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
    gamestate:verify(NativeGamestate),
    WithTurnPassed = gamestate:pass_turn(NativeGamestate),
    thrift_helper:gamestate_to_thrift(WithTurnPassed).


%% play_move :: [ThriftTile] * ThriftGamestate -> ThriftGamestate
%%
%% Given a set of tiles and a gamestate, returns a new gamestate with the tiles 
%% placed as a move.  Throws BadMoveException, or BadGamestateException if incoming 
%% data is invalid.
play_move(ThriftTiles, ThriftGamestate) ->
    debug("play_move for ~p tiles~n", [ThriftTiles]),
    try
        Tiles = lists:map(fun thrift_helper:thrift_to_native_tile/1, ThriftTiles),
        Gamestate = thrift_helper:thrift_to_gamestate(ThriftGamestate),
        gamestate:verify(Gamestate),
    
        Move = move:from_list(Tiles),
        Board = gamestate:get_gamestate_board(Gamestate),
        move:verify(Move, Board, Gamestate),
        WithMove = gamestate:play_move(Gamestate, Move),
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
    Master = get_master_gaddag(Dict),
    board:verify(NativeBoard, Master),

    Search = get_search_function(Dict),
    Moves = Search(NativeBoard, RackAsString),

    ScoreFun = get_score_function(GameName),

    WithScores = lists:map(fun (X) -> {X, ScoreFun(X, NativeBoard)} end, Moves),
    Sorted = reverse(keysort(2, WithScores)),
    lists:map(fun ({NativeMove, Score}) ->
                  Tiles = move:get_move_tiles(NativeMove),
                  ThriftTiles = lists:map(fun thrift_helper:native_to_thrift_tile/1, Tiles),
                  {move, ThriftTiles, Score}
              end, Sorted).


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


%% quit :: () -> ()
%%
%% Receive the quit notification from a client.  Should really take in a Pid or something.
quit() ->
    debug("Quit message received.~n", []),
    ok.

