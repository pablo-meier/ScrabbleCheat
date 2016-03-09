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

-module(game_parser_tests).
-include("gameinfo.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SCRABBLE_LETTER_DIST, make_dict([{$E,12},{$F,2},{$L,4},{$H,2},{$T,6},{$A,9}])).
-define(LEXULOUS_LETTER_DIST, make_dict([{$E,12},{$F,2},{$L,4},{$H,2},{$T,6},{$A,9}])).
-define(WWF_LETTER_DIST, make_dict([{$E,13},{$F,2},{$L,4},{$H,2},{$T,7},{$A,9}])).


-define(SCRABBLE_POINT_DIST, make_dict([{$E,1},{$F,4},{$L,1},{$H,4},{$T,1},{$A,1}])).
-define(LEXULOUS_POINT_DIST, make_dict([{$E,1},{$F,5},{$L,1},{$H,5},{$T,2},{$A,1}])).
-define(WWF_POINT_DIST, make_dict([{$E,1},{$F,4},{$L,2},{$H,3},{$T,1},{$A,1}])).

game_dir(Name) ->
    lists:concat([code:priv_dir(scrabblecheat), "/games/", Name, '/']).

scrabble_parse_test() ->
    Gameinfo = game_parser:parse_game_body(scrabble, game_dir("scrabble")),
    #gameinfo{board = _B, letterdist = LD, scoredist = SD, racksize = R} = Gameinfo,
    compare_dicts(LD, ?SCRABBLE_LETTER_DIST),
    compare_dicts(SD, ?SCRABBLE_POINT_DIST),
    ?assert(R =:= 7).


lexulous_parse_test() ->
    Gameinfo = game_parser:parse_game_body(lexulous, game_dir("lexulous")),
    #gameinfo{board = _B, letterdist = LD, scoredist = SD, racksize = R} = Gameinfo,
    compare_dicts(LD, ?LEXULOUS_LETTER_DIST),
    compare_dicts(SD, ?LEXULOUS_POINT_DIST),
    ?assert(R =:= 8).


wwf_parse_test() ->
    Gameinfo = game_parser:parse_game_body(words_with_friends, game_dir("words_with_friends")),
    #gameinfo{board = _B, letterdist = LD, scoredist = SD, racksize = R} = Gameinfo,
    compare_dicts(LD, ?WWF_LETTER_DIST),
    compare_dicts(SD, ?WWF_POINT_DIST),
    ?assert(R =:= 7).


make_dict(ListOfPairs) ->
    New = dict:new(),
    lists:foldl(fun ({K,V},Y) -> dict:store(K, V, Y) end, New, ListOfPairs).


%% compare_dicts :: Dict -> Dict -> Bool
compare_dicts(ToTest, Premade) ->
    Keys = dict:fetch_keys(Premade),
    lists:foreach(fun (Key) -> 
                      ?assert(dict:fetch(Key, Premade) =:= dict:fetch(Key, ToTest)) 
                  end, Keys).


%% compare_boards :: Array(Array(Tile)) -> Array(Array(Tile)) -> Bool
compare_boards() ->
    ok.


