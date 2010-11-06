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

-define(DICT_FILE, "test/testdict.txt").
-define(LARGE_DICT_FILE, "lib/twl06.txt").
-define(DICT_BIN_PATH, "build/gaddag.dict").

-export([main/0,
         make_binary_gaddag/0]).

%% Eventually the main program, right now just a testing runtime while I get
%% move generation/selection up.

%% main :: () -> IO ()
%% Test
main() ->
    greet(),
    %% Create the gaddag from the real dictionary if it exists, smaller if
    %% we built the program without it.
    Gaddag = case file:read_file_info(?DICT_BIN_PATH) of
                 {ok, _} -> dict_parser:read_from_binary(?DICT_BIN_PATH);
                 {error, _} -> parse(?DICT_FILE)
             end,
    Word_Function = get_best_move_function(Gaddag),
    Board = sample_board(),
    loop(Word_Function, Board).


make_binary_gaddag() ->
    output_to_file(?LARGE_DICT_FILE, ?DICT_BIN_PATH).

loop(Search, Board) ->
    print_board(Board),
    Chars = prompt(),
    Results = Search(Board, Chars),
    Sorted = sort_results_by_score(Results, Board),
    print_results(Sorted, Board),
    io:format("~n~n FINITO!~n"),
    case use_again() of
        true -> loop(Search, Board);
        false -> 
            io:format("Thanks!~n"),
            erlang:halt()
    end.


sample_board() ->
    board:place_word("ABLE", right, {7, 7}, board_parser:new_board()).

print_results(ResultList, Board) ->
    foreach(fun ({Score, Move}) -> io:format("---~n"), 
                print_board(place_move_on_board(Move, Board)),
                io:format("~n Score for this move: ~p~n", [Score])
            end, ResultList).


sort_results_by_score(Moves, Board) ->
    keysort(1, map(fun (X) -> {score(X, Board), X} end, Moves)).



greet() ->
    io:format("--------~nWelcome to ScrabbleCheat!~n~n").

prompt() ->
    io:format("Welcome!~n"),
    format_string_for_gaddag(io:get_line("Here is a board.  Enter some letters (your rack), see what's possible!  ")).

use_again() ->
    Answer = io:get_line("Would you like to submit again? [y/n]:  "),
    case re:run(Answer, "[yY]([Ee][sS])?") of
        {match, _Captured} -> true;
        nomatch -> false
    end.
