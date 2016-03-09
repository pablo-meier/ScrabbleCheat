-module(followstruct).

-import(board, [get_adjacent/3]).
-import(gaddag, [get_branch/2, has_branch/2, is_terminator/1, keys/1]).
-import(board, [place_letter_on_board/5, 
                orthogonals/1, 
                to_beginning/1,
                flip/1, 
                travel/4, 
                get_tile/3,
                zoom/3]).
-import(tile, [get_tile_location/1, get_tile_letter/1, is_occupied/1]).
-import(move, [add_to_move/2]).

-import(lists, [map/2, any/2, filter/2, append/2, foldl/3]).

-export([make_followstruct/5,
         get_followstruct_tile/1,
         get_followstruct_direction/1,
         get_followstruct_gaddag/1,
         get_followstruct_board/1,
         get_followstruct_move/1,
         flip_followstruct/2,
         next/3,
         can_flip_followstruct/2]).

-define(WILDCARD, $*).

%% An intermediate data type for the construction of moves.  A 'followstruct'
%% contains all the information you need to 'follow along' the GADDAG and board.
%% It consists of 
%%    - A Tile, where you are currently situated.
%%    - A Direction, where you are headed.
%%    - A Gaddag, which you use to traverse.
%%    - A Board, which you frequently reference.
%%    - A Move, which we are currently adding tiles to.
%%
%% Followstructs are created by the movesearch module.  This module simply
%% cleans out and abstracts away the details.

get_followstruct_tile({Tile, _, _, _, _}) -> Tile.
get_followstruct_direction({_, Direction, _, _, _}) -> Direction.
get_followstruct_gaddag({_, _, Gaddag, _, _}) -> Gaddag.
get_followstruct_board({_, _, _, Board, _}) -> Board.
get_followstruct_move({_, _, _, _, Move}) -> Move.


%% make_followstruct :: Tile * Direction * Gaddag * Board * Move -> FollowStruct
%%
%% Creates a new followstruct.  Silly at this point, but abstraction never
%% hurts, right?
make_followstruct(Tile, Direction, Gaddag, Board, Move) -> {Tile, Direction, Gaddag, Board, Move}.


%% flip_followstruct :: FollowStruct * Tile -> FollowStruct
%%
%% Adjusts the followstruct to 'flipping' its orientation, and accepting
%% a Zoomtile as its next cursor position.  Returns 'none' if it can't travel
%% beyond the edge of the board when it tries to flip.
flip_followstruct({_, Direction, Gaddag, Board, Move}, ZoomTile) ->
    NewDirection = flip(Direction),
    NextTile = get_adjacent(ZoomTile, Board, NewDirection),
    case get_branch($&, Gaddag) of
        {branch, NewPath} -> 
            case NextTile of
                none -> throw(try_to_flip_past_board_edge);
                _Else -> make_followstruct(NextTile, NewDirection, NewPath, Board, Move)
            end;
        none -> throw(no_flip_path_in_gaddag)
    end.


%% can_flip_followstruct :: Followstruct -> Bool
can_flip_followstruct({_, Direction, Gaddag, Board, _}, ZoomTile) ->
    NewDirection = flip(Direction),
    NextTile = get_adjacent(ZoomTile, Board, NewDirection),
    case {has_branch($&, Gaddag), NextTile} of
        {false, _} -> false;
        {_, none} -> false;
        _Else -> true
    end.

%% next :: FollowStruct * Char -> {success, FollowStruct, Moves} | fail
%%
%% See check_followstruct_on_char for the more detailed analysis on this function:
%% this function is but an entry point that determines whether we're working with
%% individual characters or a wildcard.
next(Followstruct, Char, Master) ->
    if
        Char =:= ?WILDCARD ->
            Gaddag = get_followstruct_gaddag(Followstruct),
            Keys = keys(Gaddag),
            Results = map(fun (X) -> check_followstruct_on_char(Followstruct, X, Master, true) end, Keys),
            Accumed = foldl(fun (X, Y) -> 
                          case X of
                              fail -> Y;
                              {_, NewFollow, Completed} ->
                                  {_, TotalFollow, TotalComplete} = Y,
                                  {success, [NewFollow|TotalFollow], append(Completed, TotalComplete)}
                          end
                      end, {success, [], []}, Results),
            {_, Followlist, Complete} = Accumed,
            {wildcard, Followlist, Complete};
        true ->
            check_followstruct_on_char(Followstruct, Char, Master, false)
    end.


%% check_followstruct_on_char :: Followstruct * Char * Gaddag -> {success, Followstruct, [Move]} | {wildcard, [Followstruct], [Move]} |fail 
%%
%% Travels along the FollowStruct + Gaddag, after the 'presumable' placement
%% of a character in a move.  Returns the new 'moved' followstruct and any completed
%% moves if successful, and 'fail' if a move isn't present with that char.
check_followstruct_on_char(Followstruct, Char, Master, IsWildcard) ->
    {Tile, Direction, Gaddag, Board, Move} = Followstruct,
    case tile:is_occupied(Tile) of
        true ->  
            NextTile = get_adjacent(Tile, Board, Direction),
            CanAdvance = get_branch(get_tile_letter(Tile), Gaddag),
            case {NextTile, CanAdvance} of 
                {_, none} -> fail;
                {none, _} -> fail;
                {_Next, {branch, Further}} ->
                    NewFollow = make_followstruct(NextTile, Direction, Further, Board, Move),
                    next(NewFollow, Char, Master)
            end;
        false ->
            HasBranch = get_branch(Char, Gaddag),
            WorksOrthogonally = check_other_directions(Followstruct, Char, Master),
            case {HasBranch, WorksOrthogonally} of
                {none, _}  -> fail;
                {_, false} -> fail;
                {{branch, NextPath}, _} ->
                    {Row, Col} = get_tile_location(Tile),
                    NewBoard = place_letter_on_board(Row, Col, Char, Board, IsWildcard),
                    NewMove = add_to_move(board:get_tile(Row, Col, NewBoard), Move),
                    Complete = check_completeness(Tile, NextPath, NewMove, NewBoard, Direction),
                    NewTile = get_adjacent(Tile, Board, Direction),
                    NewFollow = make_followstruct(NewTile, Direction, NextPath, NewBoard, NewMove),
                    case NewTile of
                        none -> fail;
                        _Else -> {success, NewFollow, Complete}
                    end
            end
    end.
    

%% check_completeness :: Tile * Gaddag * Move * Board * Direction -> [Move]
%%
%% Checks whether the addition of the new tile forms a new complete move or 
%% not.  If it doesn't, we return an empty list (since it contributes no new moves).
%% If it does, we return a list of all the moves the new tile forms.
check_completeness(Tile, Gaddag, Move, Board, Direction) ->
    NextTile = get_adjacent(Tile, Board, Direction),
    if 
        NextTile =:= none -> case is_terminator(Gaddag) of true -> [Move]; false -> [] end;
        true ->
            case is_occupied(NextTile) of
                true -> trace_occupied_tiles_with_gaddag(NextTile, Board, Direction, Gaddag, Move);
                false ->
                    case is_terminator(Gaddag) of true -> [Move]; false -> [] end
            end
    end.


%% trace_occupied_tiles_with_gaddag :: Tile * Board * Direction * Gaddag * Move -> [Move]
%%
%% Like board:travel/4 and board:zoom/3, we go along the board until the next adjacent
%% tile is empty, using the supplied GADDAG as a guide.  If the gaddag doesn't contain
%% a pertinent branch, or the move doesn't end in a terminator, we don't consider the move
%% complete.  Note that this does NOT verify whether or not a move is valid between 
%% 'islands,' that is done by subsequent calls to next().  We simply check whether a move
%% should be called "complete."
trace_occupied_tiles_with_gaddag(Tile, Board, Direction, Gaddag, Move) ->
    case is_occupied(Tile) of
        true -> 
            NextTile = get_adjacent(Tile, Board, Direction),
            case NextTile of
                none -> case is_terminator(Gaddag) of true -> [Move]; false -> [] end;
                _Else ->
                    Char = tile:get_tile_letter(Tile),
                    case get_branch(Char, Gaddag) of
                        none -> [];
                        {branch, Further} ->
                            trace_occupied_tiles_with_gaddag(NextTile, Board, Direction, Further, Move)
                    end
            end;
        false ->
            case is_terminator(Gaddag) of true -> [Move]; false -> [] end
    end.
    

%% check_other_directions :: Followstuct * Char -> Bool
%%
%% Checks all 'other' directions than the primary move-generating one 
%% and ensures that they form valid words all around.  Does this by
%% finding perpendicular tiles, zooming as far back as the board allows,
%% then traveling forward and ensuring the word is a complete one.
check_other_directions(Followstruct, Char, Master) ->
    %% get the orthogonal direction.
    {Tile, Direction, _, Board, _} = Followstruct,
    CheckDirections = orthogonals(Direction),
    Orthogonals = map(fun (X) -> get_adjacent(Tile, Board, X) end, CheckDirections),
    Filtered = filter(fun (X) -> X =/= none end, Orthogonals),
    Occupied = any(fun (X) -> is_occupied(X) end, Filtered),
    case Occupied of
        false -> true;
        %% if either is occupied, 
        true -> 
            %%   Place letter on a board,
            {Row, Col} = get_tile_location(Tile),
            NewBoard = place_letter_on_board(Row, Col, Char, Board, false),

            %%   zoom as far back as you can,
            BackDirection = to_beginning(hd(CheckDirections)),
            BeginTile = zoom(get_tile(Row, Col, NewBoard), BackDirection, NewBoard),
            Forwards = flip(BackDirection),

            %%   then travel as far forward as you can.
            case travel_past_separator(BeginTile, Forwards, Master, NewBoard) of
                fail -> false;
                AFollowstruct ->
                    CheckGaddag = get_followstruct_gaddag(AFollowstruct),
                    is_terminator(CheckGaddag)
            end
    end.


%% An awful, awful hack.  We hit a bug with board:travel/4,
%% where it doesn't travel past the separator character.  Rather
%% than do a silly index or risk breaking the program, this is a 
%% workaround that goes past the separator character, and continues
%% as normal.
travel_past_separator(Zoomtile, Direction, Gaddag, Board) ->
    Key = get_tile_letter(Zoomtile),
    {branch, WithSeparator} = get_branch(Key, Gaddag),
    {branch, NewGaddag} = get_branch($&, WithSeparator),
    NextTile = get_adjacent(Zoomtile, Board, Direction),
    travel(NextTile, Direction, NewGaddag, Board).
        
