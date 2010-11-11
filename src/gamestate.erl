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


%% serialize :: Gamestate -> String
%%
%% Turns the gamestate into a string we can pass through network connections.
serialize({gamestate, Board, Scores, Turn, History}) ->
    concat([board:serialize(Board), "#", serialize_list(Scores, fun serialize_score/1), 
            "#", Turn, "#", serialize_list(History, fun serialize_history/1)]).


%% serialize_list :: [a] * (a -> String) -> String
%%
%% Serializes a list of items according to the function passed in.  We use pipes to
%% separate list items "|".
serialize_list(Lst, Fun) ->
    concat(map(fun (X) -> concat([Fun(X), "|"]) end, Lst)).


%% deserialize_list :: String -> [String]
%%
%% Takes a list of items created by serialize_list and forms it into a list of strings.
deserialize_list(ListString) ->
    list_deserialize_helper(ListString, []).

list_deserialize_helper([], Accum) -> lists:reverse(Accum);
list_deserialize_helper(String, Accum) -> 
    {Elem, Rst} = split_with_delimeter(String, $|),
    list_deserialize_helper(Rst, [Elem|Accum]).


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
deserialize(GamestateString) ->
    {BoardString, Rst1} = split_with_delimeter(GamestateString, $#),
    {ScoreString, Rst2} = split_with_delimeter(Rst1, $#),
    {TurnString, HistoryString} = split_with_delimeter(Rst2, $#),
    make_gamestate(board:deserialize(BoardString), deserialize_score(ScoreString), TurnString, deserialize_history(HistoryString)).


deserialize_score(ScoreString) ->
    Scores = deserialize_list(ScoreString),
    map(fun (X) -> 
            {Player, Score} = split_with_delimeter(X, $$),
            {Player, list_to_integer(Score)}
        end, Scores).


deserialize_history(HistoryString) ->
    Movestrings = deserialize_list(HistoryString),
    map(fun (X) -> 
            {PlayerName, Rst} = split_with_delimeter(X, $$),
            {MoveString, Score} = split_with_delimeter(Rst, $$),
            {PlayerName, move:deserialize(MoveString), list_to_integer(Score)}
        end, Movestrings).
    

%% split_with_delimeter :: String * Char -> {String, String}
%%
%% Splits a string into its left and right components by the parametrized delimeter.
split_with_delimeter(String, Char) ->
    {Left, [_|Right]} = lists:splitwith(fun (X) -> X =/= Char end, String),
    {Left, Right}.
