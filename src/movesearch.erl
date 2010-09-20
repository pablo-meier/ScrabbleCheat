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

-module(movesearch).

-import(board, [as_list/1, get_adjacents/2]).
-import(tile, [get_tile_letter/1, get_tile_location/1]).
-export([get_best_move/2,
		generate_move_candidate_locations/1]). 

%% The 'meat' of the program, takes a board and a rack and generates the best
%% move for that player given its inputs.  The logical progression goes
%% something like this:
%%
%% get_candidate_moves :: Board -> [Candidate] 
%% 
%% From the board you get a list of candidates.  This is pretty much all squares
%% adjacent to a square already in play.
%%
%% Candidate * Rack -> [Move]
%%
%% From the Candidate, you generate all the possible moves that you can 'latch'
%% onto that candidate with your given rack.  This generates ALL the moves, we
%% can shrink it down later.
%%
%% [Move] * Board -> Move
%%
%% We then take all the moves we've generated and test them against each other
%% for fitness, and pick the best one.
get_best_move(Board, _Rack) ->
	generate_move_candidate_locations(Board).


%% generate_move_candidate_locations ::  Board -> [Candidate] 
generate_move_candidate_locations(Board) ->
	Flat = lists:flatten(as_list(Board)),
	Occupied = lists:filter(fun (X) -> get_tile_letter(X) =/= none end, Flat),
	Adjacents = lists:map(fun (X) -> get_adjacents(X, Board) end, Occupied),
	OpenFlat = lists:flatten(Adjacents),
	remove_duplicate_candidates(OpenFlat).


%% remove_duplicate_candidates :: [Tile] -> [Tile]
%%
%% removes the duplicate items of the list of candidates spaces (occurs when a 
%% tile is adjacent to two occupied tiles).
remove_duplicate_candidates(List) ->
	duplicate_remove_iterator(0, List).

duplicate_remove_iterator(Index, List) ->
	if 
		Index =:= length(List) ->
			List;
		true ->
			{Pred, [H|T]} = lists:split(Index, List),
			WithRemovals = duplicate_remove_helper(H, T, []),
			duplicate_remove_iterator(Index + 1, lists:append(Pred, WithRemovals))
	end.

duplicate_remove_helper(Compare, [], Accum) -> [Compare|Accum];
duplicate_remove_helper(Compare, [H|T], Accum) ->
	{ThisRow, ThisCol} = get_tile_location(Compare),
	{ThatRow, ThatCol} = get_tile_location(H),
	if
		ThisRow =:= ThatRow andalso ThisCol =:= ThatCol ->
			duplicate_remove_helper(Compare, T, Accum);
		true ->
			duplicate_remove_helper(Compare, T, [H|T])
	end.


%% Candidate * Rack -> [Move]
find_all_moves(_Candidate, _Rack) ->
	ok.


%% [Move] * Board -> Move
select_best_move(_Moves, _Board) ->
	ok.
