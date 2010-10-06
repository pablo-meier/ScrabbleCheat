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

-module(move_test).
-include_lib("eunit/include/eunit.hrl").

-define(TESTDICT, "test/testdict.txt").

-import(board_parser, [new_board/0]).
-import(board, [place_word/4, get_tile/3]).
-import(gaddag, [empty_gaddag/0]).
-import(tile, [get_tile_location/1, new_tile/4]).
-import(dict_parser, [parse/1]).
-import(movesearch, [generate_move_candidate_locations/1, 
					get_zoomtiles/3,
					traverse_back_to_candidate/2]). 



sample_board() ->
	Empty = new_board(),
	place_word("CARE", down, {7,7}, Empty).

candidate_location1_test() ->
	Lst = generate_move_candidate_locations(sample_board()),
	lists:foreach(fun (X) ->
					?assert(contains_tile(X, Lst))
					end, [{6,7},{11,7},{7,6},{7,8},{8,6},{8,8},{9,6},{9,8},{10,6},{10,8}]).

candidate_location2_test() ->
	Board = place_word("POPPYCOCK", right, {6,2}, new_board()),
	Lst = generate_move_candidate_locations(Board),
	lists:foreach(fun (X) ->
					?assert(contains_tile(X, Lst))
					end, [{6,1},{6,11},{7,2},{7,3},{7,4},{7,5},{7,8},{7,9},{7,10},
						{5,2},{5,3},{5,4},{5,5},{5,8},{5,9},{5,10}]).


zoomtile_first_test() ->
	Board = sample_board(),
	Gaddag = empty_gaddag(),
	SolutionPairs = [{new_tile(none,none,6,7), [{get_tile(10, 7, Board), up, Gaddag}]},
					{new_tile(none,none,7,6), [{get_tile(7, 7, Board), left, Gaddag}]},
					{new_tile(none,none,8,6), [{get_tile(8, 7, Board), left, Gaddag}]},
					{new_tile(none,none,11,7), [{get_tile(7, 7, Board), down, Gaddag}]},
					{new_tile(none,none,9,8), [{get_tile(9, 7, Board), right ,Gaddag}]}],
	lists:foreach(fun ({Candidate, Solution}) ->
					Result = get_zoomtiles(Candidate, Board, Gaddag),
					io:format("Candidate is ~p, Solution is ~p, Result is ~p~n", [Candidate, Solution, Result]),
					?assert(Result == Solution)
				end, SolutionPairs).

zoomtile_second_test() ->
	Board = board:place_word("BLE", right, {8, 8}, sample_board()),
	Gaddag = empty_gaddag(),
	SolutionPairs = [{new_tile(none,none,6,7), [{get_tile(10, 7, Board), up, Gaddag}]},
					{new_tile(none,none,7,6), [{get_tile(7, 7, Board), left, Gaddag}]},
					{new_tile(none,none,8,6), [{get_tile(8, 10, Board), left, Gaddag}]},
					{new_tile(none,none,8,11), [{get_tile(8, 7, Board), right, Gaddag}]},
					{new_tile(none,none,9,8), [{get_tile(9, 7, Board), right ,Gaddag},
												{get_tile(8, 8, Board), down, Gaddag}]},
					{new_tile(none,none,7,8), [{get_tile(7, 7, Board), right ,Gaddag},
												{get_tile(8, 8, Board), up, Gaddag}]}],
	lists:foreach(fun ({Candidate, Solution}) ->
					Result = get_zoomtiles(Candidate, Board, Gaddag),
					io:format("Candidate is ~p, Solution is ~p, Result is ~p~n", [Candidate, Solution, Result]),
					?assert(Result == Solution)
				end, SolutionPairs).

back_to_origin_test() ->
	Board = board:place_word("BLE", right, {8, 8}, sample_board()),
	Gaddag = parse(?TESTDICT),
	SolutionPairs = [{new_tile(none,none,6,7), [{{get_tile(6, 7, Board), up, none}, ignore}]},
					{new_tile(none,none,7,6), [{{get_tile(7, 6, Board), left, none}, ignore}]},
					{new_tile(none,none,8,6), [{{get_tile(8, 6, Board), left, none}, ignore}]},
					{new_tile(none,none,8,11), [{{get_tile(8, 11, Board), right, none}, ignore}]},
					{new_tile(none,none,9,8), [{{get_tile(9, 8, Board), right, none}, ignore},
												{{get_tile(9, 8, Board), down, none}, ignore}]},
					{new_tile(none,none,7,8), [{{get_tile(7, 8, Board), right, none}, ignore},
												{{get_tile(7, 8, Board), up, none}, ignore}]}],
	lists:foreach(fun ({Candidate, Solution}) ->
					Result = lists:map(fun (X) -> traverse_back_to_candidate(X, Board) end, get_zoomtiles(Candidate, Board, Gaddag)),
					io:format("Solution is ~p, Result is ~p~n", [Solution, Result]),
					?assert(compare_origin_test_lists(Solution, Result))
				end, SolutionPairs).

compare_origin_test_lists(List1, List2) ->
	lists:all(fun (X) -> lists:any(fun (Y) -> compare_origin_test_elements(X,Y) end, List2) end, List1).

%% FIXME This whole test suite is coked up from the followstruct refactor.  Refactor further!
compare_origin_test_elements({{Tile1, Direction1, _}, _}, {{Tile2, Direction2, _, _}, _}) ->
	Tile1 == Tile2 andalso Direction1 == Direction2.


%% Test whether or not the list contains the parametrized tile.
contains_tile({Row, Col}, Lst) ->
	Return = lists:any(fun (X) -> 
				{Row2, Col2} = get_tile_location(X),
				Row =:= Row2 andalso Col =:= Col2
			end, Lst),
	case Return of 
		true -> true;
		_False -> io:format("~p,~p Were not present. ~n", [Row,Col])
	end.
