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

-import(lists, [concat/1, foldl/3, map/2]).

-include("gameinfo.hrl").

-record(gamestate, {board, scores, turn, history, game, dict}).

-export([make_gamestate/6,
         play_move/2,
         get_gamestate_board/1,
         get_gamestate_scores/1,
         get_gamestate_turn/1,
         get_gamestate_history/1,
         get_gamestate_game/1,
         get_gamestate_dict/1,
         pass_turn/1,
         fresh_gamestate/1,
         fresh_gamestate/2,
         fresh_gamestate/3]).

%% A gamestate is the current state of the game between some players.  It's 
%% one of the things the server and clients pass around to each other when 
%% they're deciding what to do, and allow someone to recreate the game entirely.
%%
%% It is composed of 
%%   -  A board, with all the current tiles on it. :: Board
%%   -  The players' scores.                       :: [{String, Int}]
%%   -  A player whose turn it is                  :: String
%%   -  A game history                             :: [{Player, Move, Score}]
%%   -  The name of the game we're playing         :: scrabble | words_with_friends | lexulous
%%   -  The dictionary we're playing with          :: twl06 | sowpods | zynga
%%
%% This factors heavily into the MVC separation:  most of the controller code
%% (that on the Erlang side) will communicate to any View by serializing these
%% objects and passing them around, and save their state correspondingly.

make_gamestate(Board, Scores, Turn, History, Game, Dict) -> 
    #gamestate{board = Board, 
               scores = Scores, 
               turn = Turn, 
               history = History, 
               game = Game,
               dict = Dict}.


get_gamestate_board  (GS) -> GS#gamestate.board.
get_gamestate_scores (GS) -> GS#gamestate.scores.
get_gamestate_turn   (GS) -> GS#gamestate.turn.
get_gamestate_history(GS) -> GS#gamestate.history.
get_gamestate_game   (GS) -> GS#gamestate.game.
get_gamestate_dict   (GS) -> GS#gamestate.dict.



%% fresh_gamestate :: [String] * gamename() -> Gamestate
%%
%% Creates a gamestate representing a new game for a the specified game, where 
%% the players are indicated by the parameter.
fresh_gamestate(Players, Game, Dict) ->
    Gameinfo = game_parser:parse_game(Game),
    Board = Gameinfo#gameinfo.board,
    [First|_] = Players,
    make_gamestate(Board, map(fun (X) -> {X, 0} end, Players), First, [], Game, Dict).

%% fresh_gamestate :: Players * gamename() -> Gamestate
%%
%% Given only a game name, pick the default dictionary.
fresh_gamestate(Players, Game) -> 
    Dict = game_parser:default_dictionary(Game),
    fresh_gamestate(Players, Game, Dict).

%% fresh_gamestate :: [String] -> Gamestate
%%
%% Creates a fresh gamestate with our default game (Scrabble).
fresh_gamestate(Players) ->
    fresh_gamestate(Players, scrabble, twl06).


%% play_move :: Gamestate * Move -> Gamestate
%%
%% Returns the gamestate after a move has been played on it.
play_move(GS, Move) ->

    Board   = GS#gamestate.board,
    Scores  = GS#gamestate.scores,
    Turn    = GS#gamestate.turn,
    History = GS#gamestate.history,
    Game    = GS#gamestate.game,
    Dict    = GS#gamestate.dict,

    {PlayerScore, NewTurn} = get_score_and_next(Scores, Turn),
    MoveScore = move:score(Move, Board, Game),
    AugmentedScore = MoveScore + PlayerScore,
    NewScores = update_score(AugmentedScore, Scores, Turn),
    NewHistory = [{Turn, Move, MoveScore}|History],
    NewBoard = board:place_move_on_board(Move, Board),
    make_gamestate(NewBoard, NewScores, NewTurn, NewHistory, Game, Dict).


%% pass_turn :: Gamestate -> Gamestate
%%
%% Allows a player to pass if they can't find a valid move to play. Also 
%% if a challenge goes horribly awry.
pass_turn(GS) ->
    Board   = GS#gamestate.board,
    Scores  = GS#gamestate.scores,
    Turn    = GS#gamestate.turn,
    History = GS#gamestate.history,
    Game    = GS#gamestate.game,
    Dict    = GS#gamestate.dict,

    {_, NewTurn} = get_score_and_next(Scores, Turn),
    NewHistory = [{Turn, move:new_move(), 0}|History],

    make_gamestate(Board, Scores, NewTurn, NewHistory, Game, Dict).


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


