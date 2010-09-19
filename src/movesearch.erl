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

%% The 'meat' of the program, takes a board and a rack and generates the best
%% move for that player given its inputs.  The logical progression goes
%% something like this:
%%
%% get_candidate_moves :: Board -> [Candidate] 
%% 
%% From the board you get a list of candidates.  This is pretty much all squares
%% adjacent to a square already in play.
%%
%% Candidate * Rack -> [Move]
%%
%% From the Candidate, you generate all the possible moves that you can 'latch'
%% onto that candidate with your given rack.  This generates ALL the moves, we
%% can shrink it down later.
%%
%% [Move] * Board -> Move
%%
%% We then take all the moves we've generated and test them against each other
%% for fitness, and pick the best one.


%% generate_candidate_moves ::  Board -> [Candidate] 
generate_move_candidate_locations(Board) ->
	ok.


%% Candidate * Rack -> [Move]
find_all_moves(Candidate, Rack) ->
	ok.


%% [Move] * Board -> Move
select_best_move(Moves, Board) ->
	ok.
