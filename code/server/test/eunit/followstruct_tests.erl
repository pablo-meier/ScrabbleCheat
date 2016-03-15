-module(followstruct_tests).
-include_lib("eunit/include/eunit.hrl").

-import(game_parser, [new_board/0]).
-import(board, [get_tile/3, place_word/4]).
			
-import(move, [new_move/0]).
-import(dict_parser, [parse/1]).
-import(gaddag, [has_branch/2, get_branch/2, get_branch_from_string/2]).
-import(tile, [get_tile_letter/1, get_tile_bonus/1, get_tile_location/1]).

-import(followstruct, [make_followstruct/5, 
						flip_followstruct/2, 
						next/3, 
						get_followstruct_tile/1, 
						get_followstruct_direction/1, 
						get_followstruct_gaddag/1,
						get_followstruct_board/1]).

%% Rebar runs Eunit tests from a .eunit directory;  cd out, maybe
%% later find a way to more cleanly set $PROJECT_HOME or some such
%% var.
-define(TESTDICT, lists:concat([code:priv_dir(scrabblecheat), "/testdict.dict"])).


get_fixture_gaddag() ->
    case whereis(giant_bintrie) of
        undefined -> ok;
        _Else -> unregister(giant_bintrie)
    end,
    bin_trie:start_from_file(?TESTDICT),
    bin_trie:get_root(twl06).




flip_horiz_test() ->
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Gaddag = get_branch_from_string("ELBA", get_fixture_gaddag()),
	Tile = get_tile(7,6, Board),
	Followstruct = make_followstruct(Tile, left, Gaddag, Board, new_move()),
	Flipped = flip_followstruct(Followstruct, get_tile(7, 10, Board)),

	FTile = get_followstruct_tile(Flipped),
	FDir = get_followstruct_direction(Flipped),
	FGaddag = get_followstruct_gaddag(Flipped),

	?assert(get_tile_location(FTile) =:= {7,11}),
	?assert(FDir =:= right),
	?assert(has_branch($R, FGaddag)).

flip_vert_test() ->
	Board = place_word("TAR", down, {7,7}, new_board()),
	Gaddag = get_branch_from_string("RAT", get_fixture_gaddag()),
	Tile = get_tile(6,7, Board),
	Followstruct = make_followstruct(Tile, up, Gaddag, Board, new_move()),
	Flipped = flip_followstruct(Followstruct, get_tile(9, 7, Board)),

	FTile = get_followstruct_tile(Flipped),
	FDir = get_followstruct_direction(Flipped),
	FGaddag = get_followstruct_gaddag(Flipped),

	?assert(get_tile_location(FTile) =:= {10,7}),
	?assert(FDir =:= down),
	?assert(has_branch($T, FGaddag)).

flip_south_border_test() ->
	Board = place_word("TANK", down, {12,1}, new_board()),
	Gaddag = get_branch_from_string("KNAT", get_fixture_gaddag()),
	Tile = get_tile(11,1, Board),
	Followstruct = make_followstruct(Tile, up, Gaddag, Board, new_move()),
	?assertException(throw, try_to_flip_past_board_edge, flip_followstruct(Followstruct, get_tile(15, 1, Board))).


next_test() ->
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Gaddag = get_branch_from_string("ELBA", get_fixture_gaddag()),
	Tile = get_tile(7,6, Board),
	Followstruct = make_followstruct(Tile, left, Gaddag, Board, new_move()),
	{success, Moved, _} = next(Followstruct, $T, Gaddag),

	FTile = get_followstruct_tile(Moved),
	FDir = get_followstruct_direction(Moved),
	FGaddag = get_followstruct_gaddag(Moved),

	?assert(get_tile_location(FTile) =:= {7,5}),
	?assert(FDir =:= left),
	?assert(has_branch($&, FGaddag)).

