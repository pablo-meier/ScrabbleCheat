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

-module(main).

-import(movesearch, [get_best_move_function/1]).
-import(dict_parser, [parse/1, output_to_file/2]).
-import(board, [print_board/1, place_move_on_board/2]).
-import(string_utils, [format_string_for_gaddag/1]).
-import(move, [score/2]).
-import(lists, [reverse/1,foreach/2, keysort/2, sort/2, map/2]).
-import(board_parser, [new_board/0]).

-define(DICT_FILE, "test/testdict.txt").
-define(LARGE_DICT_FILE, "lib/twl06.txt").
-define(DICT_BIN_PATH, "build/gaddag.dict").
-define(WILDCARD, $*).
-define(SMALLEST_ASCII_CHARACTER, 33).
-define(LARGEST_ASCII_CHARACTER, 126).
-define(RACK_MAX_LENGTH, 7).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(PORT, 8888). %% Hard coded for testing, can make this command-line option.


-include("scrabbleCheat_thrift.hrl").


-export([main/0,

        start/0,
        start/1,
        stop/1,
        handle_function/2,
        new_game/1,
        play_move/2,
        get_scrabblecheat_suggestions/2,
        quit/0,
        get_master_gaddag/0,

        make_binary_gaddag/0]).


%% make_binary_gaddag :: () -> File ()
%%
%% The program can be invoked to build the data structures and save them disk
%% ahead of time.
make_binary_gaddag() ->
    output_to_file(?LARGE_DICT_FILE, ?DICT_BIN_PATH).




%% start :: () -> ()
%%
%% Starts a new ScrabbleCheat server on the default port.
start() ->
    start(?PORT).


%% start :: Int -> ()
%%
%% Starts a new ScrabbleCheat server on the parametrized port.  Follows the 
%% example of the Mighty Mighty Todd Lipcon on his Thrift tutorial.  Todd 
%% Lipcon is a Boss, if you didn't know.
start(Port) ->
    Handler = ?MODULE,
    Gaddag = case file:read_file_info(?DICT_BIN_PATH) of
                 {ok, _} -> dict_parser:read_from_binary(?DICT_BIN_PATH);
                 {error, _} -> 
                    io:format("Gaddag file not found!  Using test dictionary in meantime.~n"),
                    io:format("run `make binary-gaddag` to generate the full dictionary.~n"),
                    parse(?DICT_FILE)
             end,
    WordFunction = get_best_move_function(Gaddag),
    ets:new(globals, [set, protected, named_table, {keypos, 1}]), % {read_concurrency, true}]),
    ets:insert(globals, {search_function, WordFunction}),
    ets:insert(globals, {master_gaddag, Gaddag}),
    thrift_socket_server:start([{handler, Handler},
                                {service, scrabbleCheat_thrift},
                                {port, Port},
                                {socket_opts, [{recv_timeout, 100000}]},
                                {name, scrabbleCheat_server}]).


%% get_search_function :: () -> (Board * Rack -> [Move])
%%
%% Returns a search function that takes a board and rack, and produces
%% a list of moves.
get_search_function() ->
    [{search_function, Search}] = ets:lookup(globals, search_function),
    Search.

%% get_master_gaddag :: () -> Gaddag
%%
%% Get's a master, top-level Gaddag.  These are mostly used for verification
%% of moves and boards.
get_master_gaddag() ->
    [{master_gaddag, Gaddag}] = ets:lookup(globals, master_gaddag),
    Gaddag.


%% stop :: (or Name Pid) -> ()
%%
%% Stops the server named by the parameter, or its Pid.
stop(Server) ->
    ets:delete(globals),
    thrift_socket_server:stop(Server).


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
%% Throws BadNamelistException if the list is empty, or the player's names are
%% too long.
new_game(Playerlist) ->
    debug("New Game for ~p~n", Playerlist),
    Stringlist = lists:map(fun binary_to_list/1, Playerlist),
    validate_namelist(Stringlist),
    Gamestate = gamestate:fresh_gamestate(Stringlist),
    thrift_helper:gamestate_to_thrift(Gamestate).


%% validate_namelist :: [String] -> ()
%%
%% Checks the validity of the list of names.  If the list is empty or the names aren't
%% well formed, we throw an exception.
validate_namelist([]) -> throw(#badNamelistException{reprimand="There are no names here!"});
validate_namelist(Lst) ->
    case is_list(Lst) of
        false -> throw(#badNamelistException{reprimand="This is not a list!"});
        true ->
                lists:foreach(fun validate_length/1,  Lst),
                lists:foreach(fun (X) -> lists:foreach(fun validate_list_contents/1, X) end, Lst),
                ok
    end.

validate_list_contents(Elem) ->
    case is_integer(Elem) of
        false -> 
            Reprimand = lists:concat(["Element \"", [Elem], "\" not valid in a name."]),
            throw(#badNamelistException{reprimand=Reprimand});
        true -> 
            if 
                Elem < ?SMALLEST_ASCII_CHARACTER orelse Elem > ?LARGEST_ASCII_CHARACTER ->
                    Reprimand = lists:concat(["Character \"", [Elem], "\" not valid in a name."]),
                    throw(#badNamelistException{reprimand=Reprimand});
                true -> ok
            end
    end.

validate_length(Name) ->
    if 
        length(Name) > 15 -> 
            Message = lists:concat(["The name ", Name, " is too long!  15 Characters or less, please ^_^"]),
            throw(#badNamelistException{reprimand=Message});
        true -> ok
    end.

%% play_move :: [ThriftTile] * ThriftGamestate -> ThriftGamestate
%%
%% Given a set of tiles and a gamestate, returns a new gamestate with the tiles 
%% placed as a move.  Throws BadMoveException, or BadGamestateException if incoming 
%% data is invalid.
play_move(Tiles, Gamestate) ->
    debug("play_move for ~p tiles~n", [Tiles]),
    NativeTiles = lists:map(fun thrift_helper:thrift_to_native_tile/1, Tiles),
    NativeGamestate = thrift_helper:thrift_to_gamestate(Gamestate),
    gamestate:verify(NativeGamestate),

    Move = move:from_list(NativeTiles),
    Board = gamestate:get_gamestate_board(NativeGamestate),
    move:verify(Move, Board),
    WithMove = gamestate:play_move(NativeGamestate, Move),
    thrift_helper:gamestate_to_thrift(WithMove).


%% get_scrabblecheat_suggestions :: String * ThriftBoard -> [ThriftMove]
%%
%% Given a rack and a board, return a list of Thrift-compliant moves that 
%% clients can use.  Throws BadRackException and BadBoardException, if your
%% incoming data sucks.
get_scrabblecheat_suggestions(Rack, Board) ->
    debug("get_scrabblecheat_suggestions for rack ~p~n", [Rack]),
    RackAsString = binary_to_list(Rack),
    validate_rack(RackAsString),
    NativeBoard = thrift_helper:thrift_to_native_board(Board),
    Master = get_master_gaddag(),
    board:verify(NativeBoard, Master),
    Search = get_search_function(),
    Moves = Search(NativeBoard, RackAsString),
    WithScores = lists:map(fun (X) -> {X, move:score(X, NativeBoard)} end, Moves),
    Sorted = reverse(keysort(2, WithScores)),
    lists:map(fun ({NativeMove, Score}) ->
                  Tiles = move:get_move_tiles(NativeMove),
                  ThriftTiles = lists:map(fun thrift_helper:native_to_thrift_tile/1, Tiles),
                  {move, ThriftTiles, Score}
              end, Sorted).


validate_rack([]) -> throw({badRackException, "This rack is empty!"});
validate_rack(Rack) ->
    Len = length(Rack) =< ?RACK_MAX_LENGTH,
    Valid = lists:all(fun (X) -> (X >= $A andalso X =< $Z) orelse X =:= ?WILDCARD end, Rack),
    case {Len, Valid} of
        {true, true} -> ok;
        {false, _} -> throw({badRackException, "Rack is too long!"});
        {_, false} -> throw({badRackException, "There is an invalid character in your rack."})
    end.


%% quit :: () -> ()
%%
%% Receive the quit notification from a client.  Should really take in a Pid or something.
quit() ->
    debug("Quit message received.~n", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%          "CLASSIC" CODE KEPT UNTIL TRANSITION TO THRIFT IS OVAR!
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% main :: () -> IO ()
%% 
%% The "classic" routine using my own home-rolled hacktackular server.  We keep this
%% here for compatibility until the Thrift code can take over safely.
%% Creates a server and listens for requests for move searches.
main() ->
    Gaddag = case file:read_file_info(?DICT_BIN_PATH) of
                 {ok, _} -> dict_parser:read_from_binary(?DICT_BIN_PATH);
                 {error, _} -> parse(?DICT_FILE)
             end,
    Word_Function = get_best_move_function(Gaddag),
    start_service(Word_Function, ?PORT).


start_service(Search, Port) ->
    io:format("Starting server...~n"),
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    loop(LSocket, Search).


loop(LSocket, Search) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Handler = spawn(fun () -> handle_connection(Socket, Search) end),
    gen_tcp:controlling_process(Socket, Handler),
    loop(LSocket, Search).


%% handle_connection :: Socket * (String -> [Move]) -> ()
%%
%% The main socket handler loop, takes incoming directions from
%% the client and dispatches information appropriately.  The server
%% can take the following commands:
%%
%% - {new_game, String} -> Send back a fresh gamestate with those players.
%%
%% - {move, Gamestate, Move} -> play the move on the gamestate, return the 
%%                              updated gamestate.
%% 
%% - {ai, Gamestate, Rack} ->  Calls the movesearch algorithm on the gamestate
%%                             and rack.  Returns a list of moves and scores.
%%
%% - badmessage -> the client sent a bad, unrelated message.  We let them know 
%%                 this, and push forward.
%%
%% - closed -> terminate this process without closing the socket (it's been 
%%             closed elsewhere).
%%
%% - quit -> Close the connection, we're done.
%%
handle_connection(Socket, Search) ->
    case get_message(Socket) of
        {new_game, Players} -> 
            polite_response(Socket, gamestate:serialize(gamestate:fresh_gamestate(Players))),
            handle_connection(Socket, Search);
        {move, Gamestate, Move} ->
            WithMove = gamestate:play_move(Gamestate, Move),
            polite_response(Socket, gamestate:serialize(WithMove)),
            handle_connection(Socket, Search);
        {ai, Gamestate, Rack} ->
            io:format("Rack is ~p~n", [Rack]),
            Board = gamestate:get_gamestate_board(Gamestate),
            Moves = Search(Board, Rack),
            WithScores = lists:map(fun (X) -> {X, move:score(X, Board)} end, Moves),
            Sorted = reverse(keysort(2, WithScores)),
            polite_response(Socket, serialization:serialize_movelist(Sorted)),
            handle_connection(Socket, Search);
        {badmessage, Msg} ->
            polite_response(Socket, io_lib:format("Bad message, I don't understand ~p", [Msg]));
        closed ->
            terminate;
        quit ->
            gen_tcp:close(Socket)
    end.

get_message(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, MessageLength} ->
            gen_tcp:send(Socket, "thanks"),
            case gen_tcp:recv(Socket, list_to_integer(binary_to_list(MessageLength))) of
                {ok, Data} -> parse_message(binary_to_list(Data));
                {error, closed} -> closed
            end;
        {error, closed} ->
            closed
    end.


%% parse_message :: String -> Message
%%
%% Where Message types are described in handle_connection.
%%
%% parses strings sent from the clients into the message formats listed in
%% handle_connection.
parse_message(Data) ->
    io:format("Data is ~n~p~n", [Data]),
    {Core, Body} = serialization:split_with_delimeter(Data, $&),
    case Core of
        "new_game" ->
            Players = serialization:deserialize_list(Body, fun (X) -> X end),
            {new_game, Players};
        "ai" ->
            {GamestateString, Rack} = serialization:split_with_delimeter(Body, $&),
            Gamestate = gamestate:deserialize(GamestateString),
            {ai, Gamestate, Rack}; 
        "move" ->
            {GamestateString, Movestring} = serialization:split_with_delimeter(Body, $&),
            io:format("GamestateString is ~p, Movestring is ~p~n", [GamestateString, Movestring]),
            Gamestate = gamestate:deserialize(GamestateString),
            Move = move:deserialize(Movestring),
            {move, Gamestate, Move}; 
        "quit" -> quit;
        _Else  -> {badmessage, Data}
    end.



%% polite_response :: Socket * String -> ()
%%
%% Accomodates the 'polite_request' protocol of the Ruby client, where
%% we ask how many bytes to read, then read it.
polite_response(Socket, Message) ->
    Length = length(Message),
    gen_tcp:send(Socket, integer_to_list(Length)),
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"thanks">>} -> gen_tcp:send(Socket, Message);
        {ok, Something} -> io:format("Whoa!  Got ~p instead!~n", [Something]);
        {error, closed} -> io:format("Error!  Halting.  Goodbye ^_^~n"), terminate
    end.

