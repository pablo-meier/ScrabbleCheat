-module(move).

-define(SCRABBLE_RACK_SIZE, 7).
-define(SCRABBLE_BINGO_BONUS, 50).

-include("gameinfo.hrl").

-import(tile, [get_tile_letter/1, is_wildcard/1, get_tile_location/1, is_occupied/1, get_tile_bonus/1, duplicate_tile/2]).
-import(board, [place_move_on_board/2, to_beginning/1, orthogonals/1, get_adjacent/3, zoom/3, flip/1]).
-import(lists, [foldl/3, filter/2, any/2, map/2]).

-export([new_move/0, 
         verify/3,
         add_to_move/2, 
         score/3,
         duplicate_moves/2, 
         get_move_tiles/1, 
         from_list/1]).

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


%% score :: Move * Board * gamename() -> Int
%%
%% Scores a move given a move, current board, and Game Name. Used to be implemented
%% as a first-class function, but we move away due to a bug that was purely 
%% structural (the first-class function needed to be recursive; and dynamic recursive
%% function in Erlang are balls.  You'd have to Y-Combinator that shit up.  What I'd
%% do for a letrec.
score(Move, Board, GameName) ->
    GameInfo = gameinfo:get_gameinfo(GameName),
    ScoreDist = GameInfo#gameinfo.scoredist,
    ScoreFun = dict_as_function(ScoreDist),

    Bingos = GameInfo#gameinfo.bingo_bonuses,
    BingoBonus = get_bingo_bonus_function(dict_as_function(Bingos)),

    Lst = get_move_tiles(Move),
    
    ZoomBackDir = to_beginning(get_move_orientation(Lst)),
    MockBoard = place_move_on_board(Move, Board),
    [ATile|_] = Lst,
    StartTile = zoom(ATile, ZoomBackDir, MockBoard),
    Forward = flip(ZoomBackDir),
    
    %% Calculate the score of the original move
    Original = score_wordpath(StartTile, Forward, MockBoard, Lst, 0, [], ScoreFun),
    
    %% Calculate the score of any Perpendicular moves 
    Perpendiculars = score_perpendiculars(StartTile, Forward, MockBoard, Lst, 0, ScoreFun),
    
    Original + Perpendiculars + BingoBonus(Lst).


%% dict_as_function :: Dict<Key, Value> -> (Key -> Value)
%%
%% A dict is just a poor abstraction of a function. We just make it so ^_^
dict_as_function(Dict) ->
    fun (X) -> 
        case dict:find(X, Dict) of
            error -> 0;
            {ok, Value} -> Value
        end
    end.


%% get_bingo_bonus_function :: (Int -> Int) -> ([Tile] -> Int)
%%
%% Given a set of bonuses, produce a function that calculates the bingo bonus
%% applicable to a given move.
get_bingo_bonus_function(Bonuses) ->
    fun (Lst) ->
        Bonuses(length(Lst))
    end.


%% score_wordpath
%%  :: (Tile * Direction * Board * [Tile] * Int * [Bonus] * (String -> Int) -> Points)
%%
%% Given a function with a score distribution for the letters in the game (e.g.
%% A is 1 point, Q is 10), create a function that 'traces' the score by following
%% a path on the board.
score_wordpath(Tile, Direction, Board, MoveComponents, Accum, Bonuses, ScoreFun) ->
    TilePoints = case is_wildcard(Tile) of 
                    true -> 0; 
                    false -> ScoreFun(get_tile_letter(Tile)) 
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
                true  -> score_wordpath(NewTile, 
                                        Direction, 
                                        Board, 
                                        MoveComponents, 
                                        Accum + WithBonuses, 
                                        BonusAcc,
                                        ScoreFun);
                false -> 
                    foldl(fun (X, Y) -> X(Y) end, Accum + WithBonuses, BonusAcc)
            end
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


%% score_perpendiculars :: Tile * Direction * Board * [Tile] * Int * (Char -> Int) -> Points 
%% 
%% Follows a path, and if it sees moves in perpendicular directions, scores them.
score_perpendiculars(Tile, Direction, Board, MoveComponents, Accum, ScoreFun) ->
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
                    score_wordpath(StartTile, Forward, Board, MoveComponents, 0, [], ScoreFun)
            end,
    %% Continue if possible.
    case get_adjacent(Tile, Board, Direction) of
        none -> Points + Accum;
        NextTile -> 
            case is_occupied(NextTile) of
                false -> Points + Accum;
                true -> score_perpendiculars(NextTile, Direction, Board, MoveComponents, Points + Accum, ScoreFun)
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



%% verify :: Move * Board * Gamestate -> ()
%%
%% Throws a BadMoveException if the move isn't valid.  This can happen if it's empty,
%% or disconnected from other moves on the board.
verify(Move, Board, Gamestate) ->
    Tiles = get_move_tiles(Move),
    Dict = gamestate:get_gamestate_dict(Gamestate),
    case Tiles of
        [] -> throw_badMove("Move is empty!");
        _Else ->
            WithMove = board:place_move_on_board(Move, Board),
            try
                Gaddag = scrabblecheat_main:get_master_gaddag(Dict),
                board:verify(WithMove, Gaddag)
            catch
                {badArgsException, _} -> 
                    throw_badMove("This move doesn't work with the board supplied in the gamestate.")
            end
    end.


throw_badMove(Msg) ->
    Encoded = list_to_binary(Msg),
    throw({badArgsException, Encoded}).

