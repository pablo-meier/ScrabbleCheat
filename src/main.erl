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
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("Starting server...~n"),
    accept(LSocket, Search).


accept(LSocket, Search) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    io:format("Started!~n"),
    spawn(fun () -> loop(Socket, Search) end),
    accept(LSocket, Search).


loop(Socket, Search) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            io:format("Received ~p from a connection!~n", [Data]),
            gen_tcp:send(Socket, io_lib:format("~p#~p", [new_board(), 0])),
            loop(Socket, Search);
        {error, closed} ->
            ok
    end.

%% make_binary_gaddag :: () -> File ()
%%
%% The program can be invoked to build the data structures and save them disk
%% ahead of time.
make_binary_gaddag() ->
    output_to_file(?LARGE_DICT_FILE, ?DICT_BIN_PATH).


