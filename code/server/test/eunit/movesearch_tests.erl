-module(movesearch_tests).
-include_lib("eunit/include/eunit.hrl").

%% Rebar runs Eunit tests from a .eunit directory;  cd out, maybe
%% later find a way to more cleanly set $PROJECT_HOME or some such
%% var.
-define(TESTDICT, lists:concat([code:priv_dir(scrabblecheat), "/testdict.dict"])).

-import(game_parser, [new_board/0]).
-import(board, [place_word/4, get_tile/3]).
-import(gaddag, [get_branch_from_string/2]).
-import(tile, [get_tile_location/1, new_tile/4]).
-import(move, [new_move/0, duplicate_moves/2]).
-import(followstruct, [make_followstruct/5]).
-import(movesearch, [generate_move_candidate_locations/1, 
					get_zoomtiles/3,
					create_origin_followstructs/2,
					get_moves_from_candidate/5]). 



get_fixture_gaddag() ->
    case whereis(giant_bintrie) of
        undefined -> ok;
        _Else -> unregister(giant_bintrie)
    end,
    bin_trie:start_from_file(?TESTDICT),
    bin_trie:get_root(twl06).



sample_board() ->
	Empty = new_board(),
	place_word("CARE", down, {7,7}, Empty).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Getting candidate locations
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Zoomtiles
zoomtile_first_test() ->
	Board = sample_board(),
	Gaddag = get_fixture_gaddag(),
	SolutionPairs = [{new_tile(none,none,6,7), [get_tile(10, 7, Board)]},
					{new_tile(none,none,7,6), [get_tile(7, 7, Board)]},
					{new_tile(none,none,8,6), [get_tile(8, 7, Board)]},
					{new_tile(none,none,11,7), [get_tile(7, 7, Board)]},
					{new_tile(none,none,9,8), [get_tile(9, 7, Board)]}],
	lists:foreach(fun ({Candidate, Solution}) ->
					Result = lists:map(fun ({X, _, _}) -> X end, get_zoomtiles(Candidate, Board, Gaddag)),
					?assert(Result == Solution)
				end, SolutionPairs).

zoomtile_second_test() ->
	Board = board:place_word("BLE", right, {8, 8}, sample_board()),
	Gaddag = get_fixture_gaddag(),
	SolutionPairs = [{new_tile(none,none,6,7), [get_tile(10, 7, Board)]},
					{new_tile(none,none,7,6), [get_tile(7, 7, Board)]},
					{new_tile(none,none,8,6), [get_tile(8, 10, Board)]},
					{new_tile(none,none,8,11), [get_tile(8, 7, Board)]},
					{new_tile(none,none,9,8), [get_tile(9, 7, Board), get_tile(8, 8, Board)]},
					{new_tile(none,none,7,8), [get_tile(7, 7, Board), get_tile(8, 8, Board)]}],
	lists:foreach(fun ({Candidate, Solution}) ->
					Result = lists:map(fun ({X, _, _}) -> X end, get_zoomtiles(Candidate, Board, Gaddag)),
					?assert(Result == Solution)
				end, SolutionPairs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Move Generation
get_move_from_candidate_open_horiz_test() ->
	Direction = left,
	Gaddag = get_branch_from_string("ELBA",get_fixture_gaddag()), 
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Candidate = get_tile(7, 6, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(7, 10, Board),
	Rack = "TRS",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Should include SABLE, ABLER, TABLE, TABLES, STABLE
	Solutions = [{move, [{{character, $S}, none, {7,6}}]}, 
				{move, [{{character, $R}, none, {7,11}}]},
				{move, [{{character, $T}, none, {7,6}}]},
				{move, [{{character, $T}, none, {7,6}},{{character, $S}, none, {7,11}}]},
				{move, [{{character, $T}, none, {7,6}},{{character, $S}, none, {7,5}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


get_move_from_candidate_open_vert_test() ->
	Direction = up,
	Gaddag = get_branch_from_string("ELBA", get_fixture_gaddag()),
	Board = place_word("ABLE", down, {7,7}, new_board()),
	Candidate = get_tile(6, 7, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(10, 7, Board),
	Rack = "TRS",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Should include SABLE, TABLE, ABLER, TABLES, STABLE
	Solutions = [{move, [{{character, $S}, none, {6,7}}]}, 
				{move, [{{character, $R}, none, {11,7}}]},
				{move, [{{character, $T}, none, {6,7}}]},
				{move, [{{character, $T}, none, {6,7}},{{character, $S}, none, {11,7}}]},
				{move, [{{character, $T}, none, {6,7}},{{character, $S}, none, {5,7}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


%% Walls
left_wall_candidate_generate_test() ->
	Direction = right,
	Gaddag = get_branch_from_string("A&BLE", get_fixture_gaddag()),
	Board = place_word("ABLE", right, {7,1}, new_board()),
	Candidate = get_tile(7, 5, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(7, 1, Board),
	Rack = "TRS",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Should include SABLE, TABLE, ABLER, TABLES, STABLE
	Solutions = [{move, [{{character, $R}, none, {7,5}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


right_wall_candidate_generate_test() ->
	Direction = left,
	Gaddag = get_branch_from_string("ELBA", get_fixture_gaddag()),
	Board = place_word("ABLE", right, {7,12}, new_board()),
	Candidate = get_tile(7, 11, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(7, 15, Board),
	Rack = "TRS",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Should include SABLE, TABLE, STABLE
	Solutions = [{move, [{{character, $S}, none, {7,11}}]}, 
				{move, [{{character, $T}, none, {7,11}}]},
				{move, [{{character, $T}, none, {7,11}},{{character, $S}, none, {7,10}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


top_wall_candidate_generate_test() ->
	Direction = down,
	Gaddag = get_branch_from_string("A&BLE", get_fixture_gaddag()),
	Board = place_word("ABLE", down, {1,7}, new_board()),
	Candidate = get_tile(5,7, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(1,7, Board),
	Rack = "TRS",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Should include SABLE, TABLE, ABLER, TABLES, STABLE
	Solutions = [{move, [{{character, $R}, none, {5,7}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


bottom_wall_candidate_generate_test() ->
	Direction = up,
	Gaddag = get_branch_from_string("ELBA", get_fixture_gaddag()), 
	Board = place_word("ABLE", down, {12,7}, new_board()),
	Candidate = get_tile(11, 7, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(15, 7, Board),
	Rack = "TRS",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Should include SABLE, TABLE, STABLE
	Solutions = [{move, [{{character, $S}, none, {11,7}}]}, 
				{move, [{{character, $T}, none, {11,7}}]},
				{move, [{{character, $T}, none, {11,7}},{{character, $S}, none, {10,7}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


%% Corners
perpendicular_leftwise_test() ->

	Direction = up,
	Gaddag = get_fixture_gaddag(),
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Candidate = get_tile(7, 6, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(7, 6, Board),
	Rack = "LASSO",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Test LASSO, where second 'S' hooks onto ABLE for SABLE
	Solutions = [{move, [{{character, $L}, none, {4,6}},
						{{character, $A}, none, {5,6}},
						{{character, $S}, triple_letter_score, {6,6}},
						{{character, $S}, none, {7,6}},
						{{character, $O}, none, {8,6}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


perpendicular_underside_test() ->
	Direction = left,
	Gaddag = get_fixture_gaddag(),
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Candidate = get_tile(8, 7, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(8, 7, Board),
	Rack = "ZYGOTE",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Test ZYGOTE, where 'TE' hooks onto bottom of AB in ABLE
	Solutions = [{move, [{{character, $Z}, none, {8,3}},
						{{character, $Y}, double_letter_score, {8,4}},
						{{character, $G}, none, {8,5}},
						{{character, $O}, none, {8,6}},
						{{character, $T}, none, {8,7}},
						{{character, $E}, double_word_score, {8,8}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


perpendicular_upperside_test() ->
	Direction = left,
	Gaddag = get_fixture_gaddag(),
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Candidate = get_tile(6, 9, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(6, 9, Board),
	Rack = "ABHOR",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Test ABHOR, where 'AB' hooks onto top of LE in ABLE
	%% I made up the word AL, it's in the test dictionary
	Solutions = [{move, [{{character, $A}, none, {6,9}},
						{{character, $B}, triple_letter_score, {6,10}},
						{{character, $H}, none, {6,11}},
						{{character, $O}, none, {6,12}},
						{{character, $R}, none, {6,13}}]}],

	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).


perpendicular_rightside_test() ->
	Direction = up,
	Gaddag = get_fixture_gaddag(),
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Candidate = get_tile(7, 11, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(7, 11, Board),
	Rack = "CHRONO",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Test CHRONO, where 'R' hooks onto ABLE to form ABLER
	Solution = {move, [{{character, $C}, double_word_score, {5,11}},
						{{character, $H}, none, {6,11}},
						{{character, $R}, none, {7,11}},
						{{character, $O}, none, {8,11}},
						{{character, $N}, none, {9,11}},
						{{character, $O}, none, {10,11}}]},

	?assert(lists:any(fun (Y) -> duplicate_moves(Solution, Y) end, Run)).


%% this has been crashing in the client, worth automating
able_zygote_test() ->
	Gaddag = get_fixture_gaddag(),
	Board = place_word("ZYGOTE", right, {8,4}, place_word("ABLE", right, {7,8}, new_board())),
	Rack = "PAULIE",

    Moves = movesearch:get_all_moves(Board, Rack, Gaddag),
    ?assert(lists:any(fun (X) -> move:duplicate_moves(X, {move, [{{character, $A}, none, {9,5}}]}) end, Moves)).


%% this has been crashing in the client, worth automating
sigma_perpendicular_test() ->
	Gaddag = get_fixture_gaddag(),
	Board = place_word("ZYGOTE", right, {8,4}, place_word("ABLE", right, {7,8}, new_board())),
	Rack = "SIGMA",

    Moves = movesearch:get_all_moves(Board, Rack, Gaddag),
    ForbiddenMove = {move, [{{character, $S}, none, {8, 10}},
                            {{character, $I}, none, {9,10}},
                            {{character, $G}, triple_letter_score, {10,10}},
                            {{character, $M}, none, {11,10}},
                            {{character, $A}, none, {12,10}}]},
  
    Forbidden2 = {move, [{{character, $A}, none, {8, 10}},
                         {{character, $M}, none, {8,11}}]},
 
    ?assert(lists:all(fun (X) -> not move:duplicate_moves(X, ForbiddenMove) end, Moves)),
    ?assert(lists:all(fun (X) -> not move:duplicate_moves(X, Forbidden2) end, Moves)).


%% Another incorrect move generation, from not checking neighbors in same plane.
stigma_peps_test() ->
	Gaddag = get_fixture_gaddag(),
   	LetterPlacements = [{"ABLE", right, {7,7}}, 
                        {"Z", down, {6,9}}, 
                        {"OTYS", down, {8,9}},
                        {"IZY", right, {11,10}}],
    FirstBoard = lists:foldl(fun ({Word, Dir, Loc}, Accum) -> place_word(Word, Dir, Loc, Accum) end, new_board(), LetterPlacements), 
    Rack = "SIGMAT",

    Moves = movesearch:get_all_moves(FirstBoard, Rack, Gaddag),
    ForbiddenMove = {move, [{{character, $S}, none, {8,3}},
                            {{character, $T}, double_letter_score, {8,4}},
                            {{character, $I}, none, {8,5}},
                            {{character, $G}, none, {8,6}},
                            {{character, $M}, none, {8,7}},
                            {{character, $A}, double_word_score, {8,8}}]},

    ?assert(lists:all(fun (X) -> not move:duplicate_moves(X, ForbiddenMove) end, Moves)),
    
    NextBoard = place_word("MIR", down, {8,11}, place_word("AS", down, {12,11}, FirstBoard)),
    NewMoves = movesearch:get_all_moves(NextBoard, "SLTNPEP", Gaddag), % Salt-n-Pepa's here!
    
    Forbidden2 = {move, [{{character, $P}, none, {4,11}},
                         {{character, $E}, none, {5,11}},
                         {{character, $P}, none, {6,11}},
                         {{character, $S}, none, {7,11}}]},

    ?assert(lists:all(fun (X) -> not move:duplicate_moves(X, Forbidden2) end, NewMoves)).


island_1_test() ->
	Gaddag = get_fixture_gaddag(),
	PreBoard = place_word("BAS", down, {7,8}, place_word("TERM", right, {10,7}, new_board())),
	Board = place_word("TRA", down, {7,10}, PreBoard),
	Rack = "TARYO",

    Moves = movesearch:get_all_moves(Board, Rack, Gaddag),
    %% The word BAT, between BASE and TRAM
    Connected1 = {move, [{{character, $A}, double_letter_score, {7, 9}}]},
    %% The word TARRY, between BASE and TRAM
    Connected2 = {move, [{{character, $T}, none, {8, 7}},
                         {{character, $R}, none, {8, 9}},
                         {{character, $Y}, none, {8, 11}}]},

    ?assert(lists:any(fun (X) -> move:duplicate_moves(X, Connected1) end, Moves)),
    ?assert(lists:any(fun (X) -> move:duplicate_moves(X, Connected2) end, Moves)).


%% Bug where we forget to remove 'none' values after get_adjacent calls
%% in finding zoomtiles for candidates.  Below is a case where it failed.
candidate_on_edge_test() ->
	Gaddag = get_fixture_gaddag(),
	Board = place_word("AFOUL", down, {10,10}, new_board()),
	Rack = "SLUANIQ",

    %% Should not throw error -- function clause, get_tile_letter, [none]
    movesearch:get_all_moves(Board, Rack, Gaddag).
   

%% No Moves?
%% Crosses

%% Wildcards
wildcard_1_test() ->
   	Direction = left,
	Gaddag = get_branch_from_string("ELBA", get_fixture_gaddag()),
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Candidate = get_tile(7, 6, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(7, 10, Board),
	Rack = "*",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum, Gaddag),

	%% Test CHRONO, where 'R' hooks onto ABLE to form ABLER
	Solutions = [{move, [{{wildcard, $T}, none, {7,6}}]}, 
				 {move, [{{wildcard, $S}, none, {7,6}}]},
				 {move, [{{wildcard, $R}, none, {7,11}}]}],

	
	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS

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
