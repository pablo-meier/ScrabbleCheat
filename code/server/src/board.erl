-module(board).

-define(BOARD_HEIGHT, 15).
-define(BOARD_WIDTH, 15).

-import(tile, [get_tile_letter/1, 
               get_tile_bonus/1,
               get_tile_location/1, 
               is_occupied/1,
               set_tile_letter/2,
               set_tile_letter_type/2,
               set_tile_bonus/2]).

-import(move, [get_move_tiles/1]).
-import(gaddag, [get_branch/2]).
-import(followstruct, [make_followstruct/5]).
-import(move, [new_move/0]).
-import(lists, [flatten/1]).

-export([place_bonus_on_board/4,
         place_letter_on_board/5,
         place_move_on_board/2,
         get_tile/3,
         print_board/1,
         travel/4,
         zoom/3,
         get_adjacent/3,
         orthogonals/1,
         to_beginning/1,
         flip/1,
         verify/2,
         get_adjacents/2,
         place_word/4,
         as_list/1,
         from_list/1]).

%% The actual board datatype.  Queried lots to generate moves.

%% place_letter_on_board :: Int * Int * Char * Board -> Board
%%
%% The primary place where moves are employed, adds a letter to the board.
%% Returns 'fail' if the board location has been occupied by another letter tile.
place_letter_on_board(Row, Col, Char, Board, IsWildcard) ->
    TileType = if IsWildcard -> wildcard; true -> character end,
    place_letter_on_board_with_tiletype(Row, Col, Char, Board, TileType).


place_letter_on_board_with_tiletype(Row, Col, Char, Board, TileType) ->
    {RowIndex, ColIndex} = make_array_indices(Row, Col),
    ThisRow = array:get(RowIndex, Board),
    Tile = array:get(ColIndex, ThisRow),
    case get_tile_letter(Tile) of
        none ->
            WithLetter = set_tile_letter(Char, Tile), 
            Finished = set_tile_letter_type(TileType, WithLetter),
            NewRow = array:set(ColIndex, Finished, ThisRow),
            array:set(RowIndex, NewRow, Board);
        Else ->
            throw({tile_already_occupied, Else})
    end.


%% place_bonus_on_board :: Int * Int * Tile * Board -> Board
%%
%% Places a tile on the board, returns a new board with the
%% tile in place.  This is also where %% we enforce 1-indexing, rather than
%% indexing.  This is used in board creation, because it allows you to change a
%% tile's bonus.  For placement of letter, see place_letter_on_board/4
place_bonus_on_board(Row, Col, Bonus, Board) ->
    {RowIndex, ColIndex} = make_array_indices(Row, Col),
    ThisRow = array:get(RowIndex, Board),
    Tile = array:get(ColIndex, ThisRow), 
    case get_tile_bonus(Tile) of 
        none ->
            NewRow = array:set(ColIndex, set_tile_bonus(Bonus, Tile), ThisRow),
            array:set(RowIndex, NewRow, Board);
        ABonus ->
            throw({tile_already_with_bonus, ABonus})
    end.


%% place_move_on_board :: Move * Board -> Board
%%
%% Returns a board with a full move placed on it.
place_move_on_board(Move, Board) ->
    Tiles = get_move_tiles(Move),
    lists:foldl(fun (Tile, AccBoard) ->
                    {Row, Col} = get_tile_location(Tile),
                    Char = get_tile_letter(Tile),
                    place_letter_on_board(Row, Col, Char, AccBoard, false)
                end, Board, Tiles).


%% get_tile :: Int * Int * Board -> Tile
get_tile(Row, Col, Board) ->
    if 
        Row < 1 orelse Col < 1 orelse Row > ?BOARD_HEIGHT orelse Col > ?BOARD_HEIGHT ->
            throw({tile_request_out_of_bounds, Row, Col});
        true ->
            {RowIndex, ColIndex} = make_array_indices(Row, Col),
            RowArray = array:get(RowIndex, Board),
            array:get(ColIndex, RowArray)
    end.


%% get_adjacent :: Tile * atom(Direction) -> Tile | none
%% 
%% Returns the tile that is adjacent to the parametrized tile,
%% in the direction specified by the paramter (left, down, right, up).
%% Returns none if the tile is a border case.
get_adjacent(Tile, Board, left) -> 
    {Row, Col} = get_tile_location(Tile),
    if 
        Col =:= 1 -> 
            none;
        Col < 16 ->
            get_tile(Row, Col - 1, Board)
    end;
get_adjacent(Tile, Board, right) -> 
    {Row, Col} = get_tile_location(Tile),
    if 
        Col =:= 15 -> 
            none;
        Col > 0 ->
            get_tile(Row, Col + 1, Board)
    end;
get_adjacent(Tile, Board, up) -> 
    {Row, Col} = get_tile_location(Tile),
    if 
        Row =:= 1 -> 
            none; 
        Row < 16 -> 
            get_tile(Row - 1, Col, Board)
    end;
get_adjacent(Tile, Board, down) -> 
    {Row, Col} = get_tile_location(Tile),
    if 
        Row =:= 15 -> 
            none;
        Row > 0 ->
            get_tile(Row + 1, Col, Board)
    end.


%% get_adjacents :: Tile -> [Tile]
%%
%% Returns all ajacent tiles to the parametrized one.
get_adjacents(Tile, Board) ->
    AllAdjacents = lists:map(fun (X) -> get_adjacent(Tile, Board, X) end, [left,down,up,right]),
    lists:filter(fun (X) -> X =/= none end, AllAdjacents).


%% as_list :: Board -> [[Tile]]
%%
%% Returns the board as a list of lists.
as_list(Board) ->
    lists:map(fun (X) -> array:to_list(X) end, array:to_list(Board)).


%% place_word :: String * Direction * {Int, Int} * Board -> Board
%%
%% Direction :: right | down
%% Places a word in the specified direction on the board.
place_word([], _, _, Board) -> Board;
place_word(Word, Direction, {Row, Col}, Board) ->
    [H|T] = Word,
    case Direction of 
        down ->
            NewBoard = place_letter_on_board(Row, Col, H, Board, false),
            place_word(T, down, {Row + 1, Col}, NewBoard);
        right ->
            NewBoard = place_letter_on_board(Row, Col, H, Board, false),
            place_word(T, right, {Row, Col + 1}, NewBoard)
    end.


%% flip :: Direction -> Direction
%%
%% Provides the inverse of a direction.
flip(up) -> down;
flip(down) -> up;
flip(left) -> right;
flip(right) -> left.


%% orthogonals :: Direction -> [Direction]
%%
%% Provides perpendicular directions to the parameter.
orthogonals(left) -> [up, down];        orthogonals(right) -> [up,down];
orthogonals(up)   -> [left, right];     orthogonals(down)  -> [left,right];

orthogonals(horizontal) -> [up, down];  orthogonals(vertical) -> [left, right].



%% to_beginning :: Direction -> Direction
%%
%% Given a direction, returns the value that points to the beginning of a word.
to_beginning(left) -> left;         to_beginning(right) -> left;
to_beginning(up)   ->   up;         to_beginning(down)  -> up;

to_beginning(horizontal) -> left;   to_beginning(vertical) -> up.


%% zoom :: Tile * Direction * Board -> Tile
%%
%% Zooms starting from parametrized tile in the direction provided until the 
%% end of the board is reached, or the tiles stop becoming occupied.  In the
%% case that no tile is occupied in that direction, returns the original tile.
zoom(Tile, Direction, Board) ->
    case get_adjacent(Tile, Board, Direction) of
        none -> Tile;
        NewTile -> 
            case is_occupied(NewTile) of
                true -> zoom(NewTile, Direction, Board);
                false -> Tile
            end
    end.


%% travel :: Tile * Direction * Gaddag * Board -> FollowStruct
%%
%% Travels along a direction, following the GADDAG as applicable (should always
%% be possible, as only valid words are present on the board).  Returns a 
%% followstruct. 
travel(ZoomTile, Direction, Gaddag, Board) ->
    case is_occupied(ZoomTile) of
        true -> 
            Key = get_tile_letter(ZoomTile),
            HasBranch = get_branch(Key, Gaddag),
            HasNext = get_adjacent(ZoomTile, Board, Direction),
            case {HasBranch, HasNext} of
                {none, _} -> fail;
                {_, none} -> fail;
                {{branch, NewGaddag}, NextTile} ->
                    travel(NextTile, Direction, NewGaddag, Board)
            end;
        false -> 
            make_followstruct(ZoomTile, Direction, Gaddag, Board, new_move())
    end.


%% from_list :: [Tile] -> Board
%%
%% Creates a board from a list of tiles.  Note that the list MUST be the correct size.
from_list(BigList) ->
    case length(BigList) of
        ?BOARD_WIDTH * ?BOARD_HEIGHT ->    
            ListOfLists = recursive_split_lists(0, BigList, []),
            ListOfArrays = lists:map(fun (X) -> array:fix(array:from_list(X)) end, ListOfLists),
            array:fix(array:from_list(ListOfArrays));
        _Else ->
            throw_badboard("Not the correct number of tiles in list to create a board.")
    end.

recursive_split_lists(?BOARD_HEIGHT, [], Accum) -> lists:reverse(Accum);
recursive_split_lists(Num, Lst, Accum) -> 
    {Fifteen, Rest} = lists:split(?BOARD_WIDTH, Lst),
    recursive_split_lists(1 + Num, Rest, [Fifteen|Accum]).


%% print_board :: Board -> ()
%%
%% Pretty prints the board.
print_board(Board) ->
    print_key(),
    AsList = array:to_list(Board),
    PrintRow = fun (Row) ->
                   lists:foreach(fun tile:print_tile/1, array:to_list(Row)),
                   io:format("~n") 
               end, 
    lists:foreach(PrintRow, AsList).

print_key() ->
    io:format("~nKey (for any character 'p'):~n  *p* -> Triple Word!~n  ^p^ -> Double Word!~n  -p- -> Triple Letter!~n  _p_ -> Double Letter~n~n").


%% make_array_indices :: Int * Int -> {Int, Int}
%%
%% A little silly, but DRY till we die.  Facilitates 1-based board
%% indexing by creating the proper indices to the arrays that underlie the
%% implementation.
make_array_indices(Num1, Num2) -> {Num1 - 1, Num2 - 1}.


%% verify :: Board * Gaddag -> ()
%%
%% Verifies that the parameter it has received is, in fact, a valid board.
%% Criteria for validity are:
%%   - Every non-empty tile is connected (no 'islands')
%%   - Every move is valid.
%% If the board is found to be invalid, we throw a badArgsException, defined
%% in the ScrabbleCheat Thrift protocol.
verify(Board, Master) ->
    check_connectedness(Board),
    check_valid_moves(Board, Master).

%% BFS from any tile, ensure that each occupied tile on board is contained in the BFS.
check_connectedness(Board) ->
    Occupied = lists:filter(fun tile:is_occupied/1, lists:flatten(as_list(Board))),
    case length(Occupied) of
        0 -> ok;
        _Else -> 
            OccupiedSet = sets:from_list(Occupied),
            EmptySet = sets:new(),
            StartPoint = hd(Occupied),
            Connected = bfs_from_tile(Board, [StartPoint], sets:new()),
            case sets:subtract(OccupiedSet, Connected) of
                EmptySet ->
                    ok;
                _False ->
                    throw_badboard("Invalid board; some tiles are disconnected from game.")
            end
    end.

bfs_from_tile(_, [], Visited) -> Visited;
bfs_from_tile(Board, Accum, Visited) ->
    Tile = hd(Accum),
    Adjacents = lists:filter(fun tile:is_occupied/1, get_adjacents(Tile, Board)),
    Newcomers = lists:filter(fun (X) -> not sets:is_element(X, Visited) end, Adjacents),
    NewAccum = lists:append(Newcomers, tl(Accum)),
    NewVisited = sets:add_element(Tile, Visited),
    bfs_from_tile(Board, NewAccum, NewVisited).


%% Go rightwards through every row, downwards through every col, make sure everything 
%% makes sense.
check_valid_moves(Board, Master) ->
    Rows = as_list(Board),
    Cols = make_cols_from_rows(Rows),
    lists:foreach(fun (Seq) -> judge_sequence(Seq, Master) end, lists:append(Rows, Cols)).

make_cols_from_rows(Rows) ->
    RowsEmpty = lists:all(fun (X) -> X =:= [] end, Rows),
    case RowsEmpty of
        true -> [];
        false -> 
            NewCol = lists:flatten(lists:map(fun (X) -> hd(X) end, Rows)),
            Rst = lists:map(fun tl/1, Rows),
            [NewCol|make_cols_from_rows(Rst)]
    end.

judge_sequence([], _) -> ok;
judge_sequence(Seq, Master) ->
    [Fst|Rst] = Seq,
    case tile:is_occupied(Fst) of
        true -> 
            PastSubsequence = subsequence_check(Seq, Master),
            judge_sequence(PastSubsequence, Master);
        false -> judge_sequence(Rst, Master)
    end.

subsequence_check([Fst|Rst], Gaddag) ->
    Letter = tile:get_tile_letter(Fst),
    HasBranch = gaddag:has_branch(Letter, Gaddag),
    IsSingleton = case Rst =:= [] of
                      true -> true;
                      false ->
                          [Nxt|_] = Rst,
                          not tile:is_occupied(Nxt)
                  end,
    case {HasBranch, IsSingleton} of
        {false, _} ->
            throw_badboard("Error with the board: Row begins with invalid character.");
        {true, false} ->
            {branch, Further} = gaddag:get_branch(Letter, Gaddag),

            case gaddag:get_branch($&, Further) of 
                {branch, Forwards} ->
                    go_forwards(Rst, Forwards);
                _Else ->
                    throw_badboard("Invalid word found on board.")
            end;
        {true, true} ->
            Rst
    end.

go_forwards(Tiles, Gaddag) ->
    Top = hd(Tiles),
    case tile:is_occupied(Top) of
       true ->
            Letter = tile:get_tile_letter(Top),
            case gaddag:has_branch(Letter, Gaddag) of
                true -> 
                    {branch, Further} = gaddag:get_branch(Letter, Gaddag),
                    go_forwards(tl(Tiles), Further);
                false ->
                    throw_badboard("Invalid word found on board")
            end;
       false -> 
           case gaddag:is_terminator(Gaddag) of
               true -> Tiles;
               false -> throw_badboard("Error with the board: Row or col ends with invalid word.")
           end
     end.


throw_badboard(Str) ->
    Encoded = list_to_binary(Str),
    throw({badArgsException, Encoded}).
