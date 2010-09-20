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

-import(board_parser, [new_board/0]).
-import(board, [place_word/4]).
-import(movesearch, [generate_move_candidate_locations/1]). 
-import(tile, [get_tile_location/1]).



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
