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

-module(move).

-define(SCRABBLE_RACK_SIZE, 7).
-define(SCRABBLE_BINGO_BONUS, 50).

-import(tile, [get_tile_letter/1, is_wildcard/1, get_tile_location/1, is_occupied/1, get_tile_bonus/1, duplicate_tile/2]).
-import(board, [place_move_on_board/2, to_beginning/1, orthogonals/1, get_adjacent/3, zoom/3, flip/1]).
-import(lists, [foldl/3, filter/2, any/2, map/2]).
-export([new_move/0, verify/2, add_to_move/2, duplicate_moves/2, get_move_tiles/1, score/2, from_list/1]).

%% The move datatype.  Checks structural integrity of moves, not
%% responsible for legal placement relative to a board, or dictionary
%% checks.  Implemented simply as a list of tuples.


%% new_move :: () -> Move
%%
%% The atom prevents move from being flattened.
new_move() -> {move, []}.


%% add_to_move :: Tile * Move -> Move
%%
%% Adds to the move, or throws an error.
add_to_move(Tile, Move) ->
    {move, MoveList} = Move,
    {Row, Col} = get_tile_location(Tile),
    WithinBounds = check_integrity(Row, Col),
    case WithinBounds of
        true -> 
                {move, [Tile|MoveList]};
        _False -> 
            throw({out_of_bounds, {Row, Col}})
    end.


%% get_move_tiles :: Move -> [Tile]
%%
%% Evaluates to the tiles that compose the move.
get_move_tiles({move, MoveList}) -> MoveList.


%% duplicate_moves :: Move * Move -> Bool
%%
%% Given 2 moves, checks if they add the same tiles in the same places.
%% Note that while it is prefereable to not generate duplicates in the 
%% first place, this might be a TODO for later.
duplicate_moves({move, MoveList1}, {move, MoveList2}) ->
    lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, MoveList2) end, MoveList1) andalso
    lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, MoveList1) end, MoveList2).


%% %% check_integrity :: Int * Int -> Bool
check_integrity(Row, Col) ->
    Row =< 15 andalso Row >= 1 andalso Col =< 15 andalso Col >= 1.


%% score :: Move * Board -> Int
%%
%% Calculates the score of a move.
score(Move, Board) ->
    %% Get the orientation, start point of a move.
    Lst = get_move_tiles(Move),

    ZoomBackDir = to_beginning(get_move_orientation(Lst)),
    MockBoard = place_move_on_board(Move, Board),
    [ATile|_] = Lst,
    StartTile = zoom(ATile, ZoomBackDir, MockBoard),
    Forward = flip(ZoomBackDir),

    %% Calculate the score of the original move
    Original = score_word_path(StartTile, Forward, MockBoard, Lst, 0, []),

    %% Calculate the score of any Perpendicular moves 
    Perpendiculars = score_perpendiculars(StartTile, Forward, MockBoard, Lst, 0),

    Original + Perpendiculars + get_bingo_bonus(Lst).


%% get_bingo_bonus :: [Tile] -> Int
%%
%% Returns bonus points if this move is a 'bingo': where it uses and player's
%% entire rack. TODO:  If we make the AI game-agnostic (say, using Lexulous rules)
%% this is a spot where we need to add a check, since some other games have more than
%% 7 tiles in a rack.
get_bingo_bonus(List) ->
    case length(List) of
        ?SCRABBLE_RACK_SIZE -> ?SCRABBLE_BINGO_BONUS;
        _Else -> 0
    end.


%% get_move_orientation :: [Tile] -> horizontal | vertical
get_move_orientation([H|T]) ->
    {Row1, _} = get_tile_location(H),
    Horizontal = lists:all(fun (X) -> {Row2,_} = get_tile_location(X), Row2 =:= Row1 end, T),
    case Horizontal of
        true -> horizontal;
        _Else -> vertical
    end.


%% from_list :: [Tile] -> Move
%%
%% Make a Move from a list of Tiles
from_list(Lst) ->
    lists:foldl(fun (T, Acc) -> move:add_to_move(T, Acc) end, move:new_move(), Lst).


%% score_perpendiculars :: Tile * Direction * Board * [Tile] * Int -> Points 
%% 
%% Follows a path, and if it sees moves in perpendicular directions, scores them.
score_perpendiculars(Tile, Direction, Board, MoveComponents, Accum) ->
    %% See if you have a perpendicular path.
    OrthogonalTiles = map(fun (X) -> get_adjacent(Tile, Board, X) end, orthogonals(Direction)),
    Filtered = filter(fun (X) -> X =/= none end, OrthogonalTiles),
    Surrounding = filter(fun (ThisTile) -> is_occupied(ThisTile) end, Filtered),
    %% Ensure it's perpendicular to a tile that's fresh in this move.
    IsNew = is_part_of_new_move(Tile, MoveComponents),
    %% Score it as a word if there exists a perpendicular path to a new tile component.
    Points = case {Surrounding, IsNew} of
                {[], _}    -> 0; 
                {_, false} -> 0; 
                _Else -> 
                    ZoomBackDir = to_beginning(hd(orthogonals(Direction))),
                    StartTile = zoom(Tile, ZoomBackDir, Board),
                    Forward = flip(ZoomBackDir),
                    score_word_path(StartTile, Forward, Board, MoveComponents, 0, [])
            end,
    %% Continue if possible.
    case get_adjacent(Tile, Board, Direction) of
        none -> Points + Accum;
        NextTile -> 
            case is_occupied(NextTile) of
                false -> Points + Accum;
                true -> score_perpendiculars(NextTile, Direction, Board, MoveComponents, Points + Accum)
            end
    end.


%% score_word_path :: Tile * Direction * Board * [Tile] * Int * [Bonus] -> Points
%%
%% Actually counts the points of every component in a path.  Handles bonuses
%% by checking if they were part of the original moves.
score_word_path(Tile, Direction, Board, MoveComponents, Accum, Bonuses) ->
    %% Score the tile you are on.
    TilePoints = case is_wildcard(Tile) of 
                    true -> 0; 
                    false -> letter_score(get_tile_letter(Tile)) 
                end,

    %% Adjust points for letter bonuses
    IsNew = is_part_of_new_move(Tile, MoveComponents),
    LetterBonus = get_tile_bonus(Tile),
    WithBonuses = case {LetterBonus, IsNew} of 
                      {double_letter_score, true} -> 2 * TilePoints;
                      {triple_letter_score, true} -> 3 * TilePoints;
                      _Else -> TilePoints
                end,

    %% Check word bonuses and whether or not they belong in the original move.
    BonusAcc = check_and_add_bonuses(Tile, MoveComponents, Bonuses),

    %% See if you can continue
    case get_adjacent(Tile, Board, Direction) of
        none -> 
            foldl(fun (X, Y) -> X(Y) end, Accum + WithBonuses, BonusAcc);
        NewTile ->
            case is_occupied(NewTile) of
                true  -> score_word_path(NewTile, Direction, Board, MoveComponents, Accum + WithBonuses, BonusAcc);
                false -> 
                    foldl(fun (X, Y) -> X(Y) end, Accum + WithBonuses, BonusAcc)
            end
    end.


check_and_add_bonuses(Tile, MoveComponents, Bonuses) ->
    IsNew = is_part_of_new_move(Tile, MoveComponents),
    case {get_tile_bonus(Tile), IsNew} of
        {triple_word_score, true} -> [fun(X) -> 3 * X end|Bonuses];
        {double_word_score, true} -> [fun(X) -> 2 * X end|Bonuses];
        _Else -> Bonuses
    end.


is_part_of_new_move(Tile, MoveComponents) ->
    any(fun (X) -> duplicate_tile(Tile, X) end, MoveComponents).



%% Currently hardcoded for standard Scrabble.  Could have just defined as a list?
%% Lazy Sunday coding, this is...
letter_score($A) -> 1;  letter_score($B) -> 3;  letter_score($C) -> 3;  letter_score($D) -> 2;
letter_score($E) -> 1;  letter_score($F) -> 4;  letter_score($G) -> 2;  letter_score($H) -> 4;
letter_score($I) -> 1;  letter_score($J) -> 8;  letter_score($K) -> 5;  letter_score($L) -> 1;
letter_score($M) -> 3;  letter_score($N) -> 1;  letter_score($O) -> 1;  letter_score($P) -> 3;
letter_score($Q) -> 10; letter_score($R) -> 1;  letter_score($S) -> 1;  letter_score($T) -> 1;
letter_score($U) -> 1;  letter_score($V) -> 4;  letter_score($W) -> 4;  letter_score($X) -> 8;
letter_score($Y) -> 4;  letter_score($Z) -> 10.


%% verify :: Move * Board -> ()
%%
%% Throws a BadMoveException if the move isn't valid.  This can happen if it's empty,
%% or disconnected from other moves on the board.
verify(Move, Board) ->
    Tiles = get_move_tiles(Move),
    case Tiles of
        [] -> throw_badMove("Move is empty!");
        _Else ->
            WithMove = board:place_move_on_board(Move, Board),
            try
                Gaddag = scrabblecheat_main:get_master_gaddag(),
                board:verify(WithMove, Gaddag)
            catch
                {badBoardException, _} -> 
                    throw_badMove("This move doesn't work with the board supplied in the gamestate.")
            end
    end.
        
throw_badMove(Msg) ->
    Encoded = list_to_binary(Msg),
    throw({badMoveException, Encoded}).

