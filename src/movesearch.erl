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

-import(board, [as_list/1, get_adjacents/2, get_adjacent/3]).
-import(tile, [get_tile_letter/1, get_tile_location/1]).
-import(move, [new_move/0, add_to_move/2]).
-export([get_best_move_function/1,
		generate_move_candidate_locations/1,
		
		get_zoomtiles/3]). 

%% The 'meat' of the program, takes a board and a rack and generates the best
%% move for that player given its inputs.  The logical progression goes
%% something like this:
%%
%% get_move_candidate_locations :: Board -> [Candidate] 
%% 
%% From the board you get a list of candidates.  This is pretty much all squares
%% adjacent to a square already in play.
%%
%% find_all_moves :: Candidate * Rack * Board * Gaddag -> [Move]
%%
%% From the Candidate, you generate all the possible moves that you can 'latch'
%% onto that candidate with your given rack.  This generates ALL the moves, we
%% can shrink it down later.
%%
%% get_best_move :: [Move] * Board -> Move
%%
%% We then take all the moves we've generated and test them against each other
%% for fitness, and pick the best one.
%%
%% Each of these steps requires lots of helpers; the functions are grouped 
%% according to these steps, and have header comments seperating them.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_best_move_function :: Gaddag -> (Board * Rack -> Move)
%%
%% We curry out the Gaddag so we make it once and don't have to worry about it.
get_best_move_function(Gaddag) ->
	fun (Board, Rack) ->
		Candidates = generate_move_candidate_locations(Board),
		MoveList = lists:flatmap(fun (X) -> find_all_moves(X, Rack, Board, Gaddag) end, Candidates),
		select_best_move(MoveList,Board)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate_move_candidate_locations ::  Board -> [Candidate] 
generate_move_candidate_locations(Board) ->
	Flat = lists:flatten(as_list(Board)),
	Occupied = lists:filter(fun (X) -> get_tile_letter(X) =/= none end, Flat),
	Adjacents = lists:map(fun (X) -> get_adjacents(X, Board) end, Occupied),
	OpenFlat = lists:filter(fun (X) -> get_tile_letter(X) =:= none end, lists:flatten(Adjacents)),
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
			duplicate_remove_helper(Compare, T, [H|Accum])
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_all_moves :: Candidate * Rack * Board -> [Move]
%%
%% The 'meat' of the search, uses the Gaddag to traverse the board for possible
%% moves.  This relies on a very tangled recursive subroutine, may God have 
%% mercy on us all.
%%
%% 'Zoomtile' is described best as the furthest tile to the right of the 'line' 
%% you are investigating.  Supposing you have ABLE, and you are investigating
%% the space to the left of 'A'.  When you hit the separator character on the
%% GADDAG, you need to know to continue forward past E, and furthermore, that
%% your word should contain all of ABLE, not just A.  So TABLEMAKER can be made
%% by assigning the ZoomTile to E, traversing the GADDAG to via 'ELBA'.  
%%
%% At that point the recursive routine can pick T from your rack and GADDAG, 
%% find the seperator, and 'jump' past the ZoomTile to complete the word 
%% (MAKER).  The idea is that you begin each search of a candidate square by
%% 'zooming' as far forward as you can, and using the Gaddag to find your way
%% back.
find_all_moves(Candidate, Rack, Board, Gaddag) ->
	ZoomTiles = get_zoomtiles(Candidate, Board, Gaddag),
	lists:flatmap(fun ({ZoomTile, Direction, Path}) -> 
					get_moves_from_candidate(Candidate, ZoomTile, Direction, Rack, Path, new_move(), [], Board)
				end, ZoomTiles).


%% get_zoomtiles :: Candidate * Board * Gaddag -> [{Tile, Direction, Gaddag}]
%%
%% Given a candidate square, checks on all sides for adjacent occupied squares.  
%% When encountered, 'zooms' as far down the Gaddag as it can until it reaches
%% the furthest progression.  Then it traverses back to the candidate square, 
%% moving along the Gaddag the whole time.
get_zoomtiles(Candidate, Board, Gaddag) ->
	Adjacents = lists:map(fun (X) -> {get_adjacent(Candidate, Board, X), X, Gaddag} end, [left,right,up,down]),
	StartPoints = lists:filter(fun ({X,_,_}) -> tile:is_occupied(X) end, Adjacents),
	WithZooms = lists:map(fun(X) -> zoom(X, Board) end, StartPoints),
	lists:filter(fun (X) -> X =/= edge_of_board end, WithZooms).


zoom({Tile, Direction, Gaddag}, Board) ->
	Adjacent = get_adjacent(Tile, Board, Direction),
	case {Tile, get_tile_letter(Adjacent)} of
		{none, _} -> edge_of_board;
		{_, none} -> {Tile, flip(Direction), Gaddag};
		_Else -> zoom({Adjacent, Direction, Gaddag}, Board)
	end.


flip(up) -> down;
flip(down) -> up;
flip(left) -> right;
flip(right) -> left.


%% get_moves_from_candidate :: {Int,Int} * Tile * Direction * [Char] * Gaddag * Move * [Move] * Board -> [Move]
%%
%% THERE BE BUGS HERE.
get_moves_from_candidate({_Row, _Col}, _ZoomTile, _Direction, _Rack, _Gaddag, _Move, _Moves, _Board) ->
%%	case board:get_tile(Row, Col, Board) of
%%		none -> Moves;
%%		Tile ->
%%			case tile:get_letter(Tile) of
%%				none ->
%%					MoveMaps = lists:map( fun (X) ->
%%											case gaddag:get_branch(X, Gaddag) of
%%												none -> false;
%%												Path ->
%%													NewRack = Rack -- [X],
%%													NewMove = move:add_to_move({Row, Col, X}, Move),
%%											end
%%										end, Rack),
%%					lists:filter(fun (X) -> X =/= false end, MoveMaps);
%%				Letter ->
%%					throw({lol_occupied, Letter})
%%			end
%%	end.
ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% select_best_move :: [Move] * Board -> Move
select_best_move(_Moves, _Board) ->
	ok.
