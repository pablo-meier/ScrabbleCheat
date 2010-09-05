-module(tile).


-define(SPACE, 32).
-export([new_tile/2, print_tile/1]).

%% Datatype for a tile, which is what the board is composed of. Keeps track of 
%% bonuses, and which letter is where.  

%% Bonus types are none, triple_letter_score, double_letter_score, 
%% triple_word_score, double_word_score.
%% 'Occupied' means, what's in the tile.  It can be none, {character, Char} or 
%% {wildcard, Char}.

new_tile(Occupied, Bonus) ->
	{Occupied, Bonus}.


print_tile({none, Bonus}) ->
	print_tile_skeleton(?SPACE, Bonus);
print_tile({Letter, Bonus}) ->
	print_tile_skeleton(Letter, Bonus).

print_tile_skeleton(Letter, Bonus) ->
	case Bonus of
		none -> io:format(" ~s ", [[Letter]]);
		triple_word_score -> io:format("*~s*", [[Letter]]);
		double_word_score -> io:format("^~s^", [[Letter]]);
		triple_letter_score -> io:format("-~s-", [[Letter]]);
		double_letter_score -> io:format("_~s_", [[Letter]]);
		_false -> uh_oh
	end.

