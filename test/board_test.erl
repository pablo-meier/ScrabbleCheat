-module(board_test).
-include_lib("eunit/include/eunit.hrl").

-import(board_parser, [new_board/0]).
-import(board, [place_letter_on_board/4,
				place_bonus_on_board/4,
				get_tile/3]).
-import(tile, [get_tile_letter/1, get_tile_bonus/1]).



placement_test() ->
	Board = new_board(),

	WithTripleLetterBonus = place_bonus_on_board(3, 2, triple_letter_score, Board),
	WithDoubleWordBonus = place_bonus_on_board(8, 11, double_word_score, Board),
	?assert(get_tile_bonus(get_tile(8, 11, WithDoubleWordBonus)) =:= double_word_score),
	?assert(get_tile_bonus(get_tile(3, 2, WithTripleLetterBonus)) =:= triple_letter_score),
	
	WithLetterC = place_letter_on_board(11, 10, $C, Board),
	WithLetterM = place_letter_on_board(13, 10, $M, Board),
	?assert(get_tile_letter(get_tile(11, 10, WithLetterC)) =:= $C),
	?assert(get_tile_letter(get_tile(13, 10, WithLetterM)) =:= $M).

preservation_test() ->
	Board = new_board(),
	WithDoubleWordBonus = place_bonus_on_board(13, 4, double_word_score, Board),

	AndWithC = place_letter_on_board(13, 4, $C, WithDoubleWordBonus),
	?assert(get_tile_bonus(get_tile(13, 4, AndWithC)) =:= double_word_score),

	?assertException(throw, {tile_already_occupied, $C}, place_letter_on_board(13, 4, $M, AndWithC)),
	?assertException(throw, {tile_already_with_bonus, double_word_score}, place_bonus_on_board(13, 4, triple_letter_score, AndWithC)).
	
