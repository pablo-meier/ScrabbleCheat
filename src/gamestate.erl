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

-module(gamestate).

-import(board_parser, [empty_board/0]).
-import(lists, [concat/1, foldl/3, map/2]).

-export([make_gamestate/4, 
         serialize/1, 
         serialize_list/2,
         deserialize/1, 
         get_gamestate_board/1,
         get_gamestate_scores/1,
         get_gamestate_turn/1,
         get_gamestate_history/1]).

%% A gamestate is the current state of the game between some players.  It's 
%% one of the things the server and clients pass around to each other when 
%% they're deciding what to do, and allow someone to recreate the game entirely.
%%
%% It is composed of 
%%   -  A board, with all the current tiles on it. :: Board
%%   -  The players' scores. :: [{String, Int}]
%%   -  A player whose turn it is :: String
%%   -  A game history :: [{Player, Move, Score}]
%%
%% This factors heavily into the MVC separation:  most of the controller code
%% (that on the Erlang side) will communicate to any View by serializing these
%% objects and passing them around, and save their state correspondingly.
%%
%% Serialization will be to strings.  Each element will be separated by a pound
%% character '#'.

make_gamestate(Board, Scores, Turn, History) -> {gamestate, Board, Scores, Turn, History}.

get_gamestate_board  ({gamestate, Board, _, _, _}) -> Board.
get_gamestate_scores ({gamestate, _, Scores, _, _}) -> Scores.
get_gamestate_turn   ({gamestate, _, _, Turn, _}) -> Turn.
get_gamestate_history({gamestate, _, _, _, History}) -> History.

serialize({gamestate, Board, Scores, Turn, History}) ->
    concat([board:serialize(Board), "#", serialize_list(Scores, fun serialize_score/1), 
            "#", Turn, "#", serialize_list(History, fun serialize_history/1)]).
 

%% serialize_list :: [a] * (a -> String) -> String
%%
%% Serializes a list of items according to the function passed in.  We use pipes to
%% separate list items "|".
serialize_list(Lst, Fun) ->
    concat(map(fun (X) -> concat([Fun(X), "|"]) end, Lst)).


%% serialize_score :: {String, Int} -> String
%%
%% Store the player name and their score.  Will follow the convention of all tuples that
%% dollar sign '$' separates members.
serialize_score({String, Int}) ->
    concat([String, "$", integer_to_list(Int)]).

%% serialize_history :: {String, Move, Int} -> String
%%
%% As serialize score, we'll separate tuple items with "$"
serialize_history({Player, Move, Score}) ->
    concat([Player, "$", move:serialize(Move), "$", integer_to_list(Score)]).


%% deserialize :: String -> Gamestate
%%
%% Turn the string back into a Gamestate.
deserialize(_GamestateString) ->
    ok.

%% empty_gamestate() ->
%%    {gamestate, empty_board(), [{"Paul", 0}, {"Sam", 0}], "Paul", []}.
