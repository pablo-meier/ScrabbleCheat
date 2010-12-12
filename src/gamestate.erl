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
-import(serialization, [serialize_list/2, deserialize_list/2, split_with_delimeter/2]).

-export([make_gamestate/4, 
         play_move/2,
         serialize/1, 
         deserialize/1, 
         get_gamestate_board/1,
         get_gamestate_scores/1,
         get_gamestate_turn/1,
         get_gamestate_history/1,
         fresh_gamestate/1]).

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


%% fresh_gamestate :: [String] -> Gamestate
%%
%% Creates a gamestate for a new game, where the players are indicated by the parameter.
fresh_gamestate(Players) ->
    [First|_] = Players,
    make_gamestate(board_parser:new_board(), map(fun (X) -> {X, 0} end, Players), First, []).


%% play_move :: Gamestate * Move -> Gamestate
%%
%% Returns the gamestate after a move has been played on it.
play_move(Gamestate, Move) ->
    {gamestate, Board, Scores, Turn, History} =  Gamestate,
    {PlayerScore, NewTurn} = get_score_and_next(Scores, Turn),
    MoveScore = move:score(Move, Board),
    AugmentedScore = MoveScore + PlayerScore,
    NewScores = update_score(AugmentedScore, Scores, Turn),
    NewHistory = [{Turn, Move, MoveScore}|History],
    NewBoard = board:place_move_on_board(Move, Board),
    make_gamestate(NewBoard, NewScores, NewTurn, NewHistory).


%% get_score_and_next :: [{String, Int}] * String -> {Int, String}
%%
%% Given a player whose turn it is and the list of players and scores,
%% get the current player's score, and the next player in line.
get_score_and_next([{FirstPlayer, FirstScore}|Rst], Turn) ->
    case Turn of
        FirstPlayer -> 
            {NextPlayer,_} = hd(Rst),
            {FirstScore, NextPlayer};
        _True ->
            get_score_and_next_helper(Rst, Turn, FirstPlayer)
        end.

get_score_and_next_helper([{CurrPlayer, CurrScore}|Rst], Turn, FirstPlayer) ->
    case Turn of
        CurrPlayer ->
            case Rst of
                [] -> {CurrScore, FirstPlayer};
                [{Next, _}|_] -> {CurrScore, Next}
            end;
        _Other ->
            get_score_and_next_helper(Rst, Turn, FirstPlayer)
    end.


%% update_score :: Int * [{String, Int}] * String -> [{String, Int}]
%%
%% Substitutes the list with one where the player whose turn it is
%% has the new score on it.
update_score(NewScore, OldList, Turn) ->
    lists:map(fun ({Name, Score}) -> 
                  case Turn of
                      Name -> {Name, NewScore};
                      _Else -> {Name, Score}
                  end
              end, OldList).


%% serialize :: Gamestate -> String
%%
%% Turns the gamestate into a string we can pass through network connections.
serialize({gamestate, Board, Scores, Turn, History}) ->
    concat([board:serialize(Board), "#", serialize_list(Scores, fun serialize_score/1), 
            "#", Turn, "#", serialize_list(History, fun serialize_history/1)]).


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
    make_gamestate(board:deserialize(BoardString), deserialize_list(ScoreString, fun deserialize_score/1), 
                   TurnString, deserialize_history(HistoryString, [])).


deserialize_score(ScoreString) ->
    {Player, Score} = split_with_delimeter(ScoreString, $$),
    {Player, list_to_integer(Score)}.


deserialize_history([], Accum) -> lists:reverse(Accum);
deserialize_history(HistoryString, Accum) ->
    {PlayerName, Rst} = split_with_delimeter(HistoryString, $$),
    {MoveString, ScoreWithHistory} = split_with_delimeter(Rst, $$),
    {Score, RestOfHistory} = split_with_delimeter(ScoreWithHistory, $|),
    HistoryEntry = {PlayerName, move:deserialize(MoveString), list_to_integer(Score)},
    deserialize_history(RestOfHistory, [HistoryEntry|Accum]).
    
