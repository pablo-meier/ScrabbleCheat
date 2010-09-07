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

