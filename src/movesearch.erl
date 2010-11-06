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

-define(SEPARATOR, $&).

-import(followstruct, [make_followstruct/5, 
                       next/3, 
                       flip_followstruct/2,
                       get_followstruct_board/1,
                       get_followstruct_gaddag/1,
                       get_followstruct_direction/1,
                       get_followstruct_tile/1,
                       can_flip_followstruct/2]).

-import(board, [as_list/1, get_adjacents/2, get_adjacent/3, get_tile/3, flip/1, zoom/3, travel/4,
                orthogonals/1, to_beginning/1]).
-import(tile, [get_tile_letter/1, get_tile_location/1, is_occupied/1]).
-import(gaddag, [get_branch/2, has_branch/2, is_terminator/1]).
-import(move, [new_move/0, add_to_move/2]).
-import(lists, [map/2, filter/2, flatmap/2, flatten/1, append/2, foldl/3]).


-export([get_best_move_function/1,
        generate_move_candidate_locations/1,
        
         get_zoomtiles/3
        , create_origin_followstructs/2
        , get_moves_from_candidate/5
        
        ]). 

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
        _MoveList = flatmap(fun (X) -> find_all_moves(X, Rack, Board, Gaddag) end, Candidates)
        %select_best_move(MoveList, Board)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate_move_candidate_locations ::  Board -> [Candidate] 
generate_move_candidate_locations(Board) ->
    Flat = flatten(as_list(Board)),
    Occupied = filter(fun (X) -> get_tile_letter(X) =/= none end, Flat),
    Adjacents = map(fun (X) -> get_adjacents(X, Board) end, Occupied),
    OpenFlat = filter(fun (X) -> get_tile_letter(X) =:= none end, flatten(Adjacents)),
    remove_duplicates(OpenFlat, fun compare_candidate/2).


%% remove_duplicate :: [a] * (a * a -> Bool) -> [a]
%%
%% Removes the duplicate items of the list, provided a function determining equality.
remove_duplicates(List, Predicate) ->
    duplicate_remove_iterator(0, List, Predicate).

duplicate_remove_iterator(Index, List, Predicate) ->
    if 
        Index =:= length(List) ->
            List;
        true ->
            {Predecessor, [H|T]} = lists:split(Index, List),
            WithRemovals = duplicate_remove_helper(H, T, [], Predicate),
            duplicate_remove_iterator(Index + 1, append(Predecessor, WithRemovals), Predicate)
    end.

duplicate_remove_helper(Compare, [], Accum, _) -> [Compare|Accum];
duplicate_remove_helper(Compare, [H|T], Accum, Predicate) ->
    case Predicate(Compare, H) of   
        true ->
            duplicate_remove_helper(Compare, T, Accum, Predicate);
        false ->
            duplicate_remove_helper(Compare, T, [H|Accum], Predicate)
    end.


%% the predicate we pass to remove_duplicates for comparing candidate spaces.
compare_candidate(Candidate1, Candidate2) ->
    {ThisRow, ThisCol} = get_tile_location(Candidate1),
    {ThatRow, ThatCol} = get_tile_location(Candidate2),
    ThisRow =:= ThatRow andalso ThisCol =:= ThatCol.

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
%% by assigning the ZoomTile to E ("zooming" to the right as far as you can), 
%% traversing the GADDAG through the string 'ELBA'.  
%%
%% At that point the recursive routine can pick T from your rack and GADDAG, 
%% find the seperator, and 'jump' past the ZoomTile to complete the word 
%% (MAKER).  The idea is that you begin each search of a candidate square by
%% 'zooming' as far forward as you can, and using the Gaddag to find your way
%% back, THEN traversing the GADDAG greedily using backtracking and 
%% accumulators to slowly build up a move.
find_all_moves(Candidate, Rack, Board, Gaddag) ->
    ZoomTiles = get_zoomtiles(Candidate, Board, Gaddag),
    StartLocations = map(fun (X) -> create_origin_followstructs(X, Board) end, ZoomTiles),
    Perpendiculars = map(fun ({Followstruct, _}) -> make_perpendicular_followstructs(Followstruct, Gaddag) end, StartLocations),
    Total = append(Perpendiculars, StartLocations),
    flatmap(fun ({FollowStruct, ZoomTile}) -> 
                get_moves_from_candidate(FollowStruct, ZoomTile, Rack, [], Gaddag)
            end, Total).


%% get_zoomtiles :: Candidate * Board * Gaddag -> [{Tile, Direction, Gaddag}]
%%
%% Given a candidate square, checks on all sides for adjacent occupied squares.  
%% When encountered, 'zooms' as far down the Gaddag as it can until it reaches
%% the furthest progression.  We don't travel back up with the Gaddag until
%% we create the appropriate Followstructs.
get_zoomtiles(Candidate, Board, Gaddag) ->
    Adjacents = map(fun (X) -> {get_adjacent(Candidate, Board, X), X, Gaddag} end, [left,right,up,down]),
    StartPoints = filter(fun ({X,_,_}) -> is_occupied(X) end, Adjacents),
    WithZooms = map(fun({Tile, Direction, _}) -> {zoom(Tile, Direction, Board), Direction, Gaddag} end, StartPoints),
    filter(fun (X) -> X =/= edge_of_board end, WithZooms).


%% create_origin_followstructs :: {ZoomTile, Direction, Gaddag} -> {FollowStruct, ZoomTile}
%%
%% From the Zoomtile, we traverse the GADDAG back to the origin candidate 
%% location, getting ready to start building words.  Note that we split the forward-travel 
%% directions (right, down) into separate cases since the follow-branch model breaks when you 
%% want a simple forward word (P&AUL).  We just get past the separator (&) and move on.
create_origin_followstructs(ThisTriple, Board) ->
    {ZoomTile, Direction, Gaddag} = ThisTriple,
    if
        Direction =:= right orelse Direction =:= down ->
            {travel(ZoomTile, flip(Direction), Gaddag, Board), ZoomTile};   
        Direction =:= left orelse Direction =:= up ->
            NewDirection = flip(Direction),
            {branch, NewGaddag} = get_branch(get_tile_letter(ZoomTile), Gaddag),
            NextTile = get_adjacent(ZoomTile, Board, NewDirection),
            {branch, GoForward} = get_branch(?SEPARATOR, NewGaddag),
            {travel(NextTile, NewDirection, GoForward, Board), ZoomTile}
    end.


%% get_moves_from_candidate :: FollowStruct * Tile * [Char] * Move * [Move] -> [Move]
%%
%% Given all the information, construct every possible move given your
%% rack and the board by following using your followstruct, containing direction.
get_moves_from_candidate(Followstruct, ZoomTile, Rack, Accum, Master) ->
    ListOfMoves = case can_flip_followstruct(Followstruct, ZoomTile) of
        false -> get_moves_from_candidate_recur(Followstruct, ZoomTile, Rack, Accum, Master);
        true -> append(get_moves_from_candidate_recur(Followstruct, ZoomTile, Rack, Accum, Master),
                        get_moves_from_candidate_recur(flip_followstruct(Followstruct, ZoomTile), ZoomTile, Rack, Accum, Master))
    end,
    remove_duplicates(ListOfMoves, fun move:duplicate_moves/2).


%% make_perpendicular_followstructs :: Followstruct * Gaddag -> {FollowStruct, Tile}
%%
%% Gives us a start point from which to generate moves that hook in a perpendicular
%% fashion, rather than in the direction facing outward.
make_perpendicular_followstructs(Followstruct, Master) ->
    Tile = get_followstruct_tile(Followstruct),
    Board = get_followstruct_board(Followstruct),

    Direction = get_followstruct_direction(Followstruct),
    [Perpendicular|_] = orthogonals(Direction),
    BackPerpendicular = to_beginning(Perpendicular),
    {make_followstruct(Tile, BackPerpendicular, Master, Board, new_move()), Tile}.


%% The hairiest part of the movesearch algorithm.  The rest is just foreplay...
get_moves_from_candidate_recur(Followstruct, ZoomTile, Rack, Accum, Master) ->
    %% Fold the results into a unified list.  For every character in your rack...
    foldl(fun (X, Y) -> 
              %% See if you can place that character per the data in the followstruct.
              case next(Followstruct, X, Master) of
                  %% If so ...
                  {success, NewFollowstruct, Complete} ->
                      %% Remove it from your rack.
                      RestOfRack = Rack -- [X],
                      %% Take any moves resulting from that tile, and add it to the list.
                      NewAccum = append(Complete, Y),
                      %% If you can swap to the other side,
                      case can_flip_followstruct(NewFollowstruct, ZoomTile) of
                          true ->
                              %% Do so, and append the results of both the backwards and forwards direction.
                              BranchFollowstruct = flip_followstruct(NewFollowstruct, ZoomTile),
                              foldl(fun (S,T) -> 
                                        get_moves_from_candidate_recur(S, ZoomTile, RestOfRack, T, Master)
                                    end, NewAccum, [NewFollowstruct, BranchFollowstruct]);
                          false ->
                              %% Otherwise, just continue forward.
                              get_moves_from_candidate_recur(NewFollowstruct, ZoomTile, RestOfRack, NewAccum, Master) 
                      end;
                  %% In the case of a wildcard
                  {wildcard, FollowList, Complete} ->
                      ok;
                  %% Else, return the moves you already have.
                  fail -> Y
              end
        end, Accum, Rack).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% select_best_move :: [Move] * Board -> Move
%% select_best_move(_Moves, _Board) ->
%%  ok.
