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

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(PORT, 6655). %% Hard coded for testing, can make this command-line option.

-export([main/0,
         make_binary_gaddag/0]).

%% Eventually the main program, right now just a testing runtime while I get
%% move generation/selection up.




%% main :: () -> IO ()
%% 
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
%% NOTE:  After a terrible, stupid blip in my memory, I completely forgot
%%      that Thrift exists for this very problem.  I'm so far down it that
%%      I've resolved to finish this way, but note that in the future, I'm
%%      going to turn it back at some point.  That being said, Thrift doesn't
%%      have AS bindings, which may make it hard to build an AIR wrapper...
%%      maybe I should add?
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

%% make_binary_gaddag :: () -> File ()
%%
%% The program can be invoked to build the data structures and save them disk
%% ahead of time.
make_binary_gaddag() ->
    output_to_file(?LARGE_DICT_FILE, ?DICT_BIN_PATH).


