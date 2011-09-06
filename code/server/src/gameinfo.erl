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

-module(gameinfo).
-include("gameinfo.hrl").

-behaviour(gen_server).

%% module API
-export([get_gameinfo/1,
        default_dictionary/1]).

%% gen_server API
-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% external API
-export([start_link/0,
         start_link_from_paths/1,
         start_from_paths/1,
         start/0]).

%% This module acts as an easy way for other processes and modules to access
%% game info at any time that they need it (scrabblecheat_main for 
%% verification, move for scoring, etc. etc.)



%% default_dictionary :: gamename() -> dict()
%%
%% Given a game name, retrieve the name of that game's default dictionary.
default_dictionary(Game) ->
    GameInfo = get_gameinfo(Game),
    hd(GameInfo#gameinfo.allowed_dicts).


get_gameinfo(GameName) ->
    {ok, Gameinfo} = gen_server:call(gameinfos, {get_gameinfo, GameName}),
    Gameinfo.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STARTING/STOPPING 


make_state() ->
    GameInfos = lists:map(fun (X) -> {X, game_parser:parse_game(X)} end, 
                          [scrabble, lexulous, words_with_friends]),
    lists:foldl(fun({K,V}, Ord) -> orddict:store(K,V,Ord) end, orddict:new(), GameInfos).


start_link() ->
    State = make_state(),
    gen_server:start_link({local, gameinfos}, ?MODULE, State, []).

start() ->
    State = make_state(),
    gen_server:start({local, gameinfos}, ?MODULE, State, []).


%% These methods allow one to specify a filename from which to load, rather than
%% doing it dynamically with code. This is only really useful for unit testing, 
%% since code:priv_dir doesn't resolve unless you package a release.
-spec start_link_from_paths([{atom(), string()}]) -> pid.
start_link_from_paths(NamePathLst) ->
    gen_server:start_link({local, gameinfos}, ?MODULE, namepath_state(NamePathLst), []).

start_from_paths(NamePathLst) ->
    gen_server:start({local, gameinfos}, ?MODULE, namepath_state(NamePathLst), []).


namepath_state(NamePathLst) ->
    KeyVals = lists:map(fun({Name,Path}) -> 
                            {Name, game_parser:parse_game_body(Name, Path)} 
                        end, NamePathLst),
    lists:foldl(fun({K,V}, Ord) -> orddict:store(K,V,Ord) end, orddict:new(), KeyVals).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER BEHAVIOR

%% init :: [Args] -> {ok, State} | {stop, Reason}
init(Args) ->
    {ok, Args}.

%% fetch :: char() * bintrie() -> bintrie()
%%
%% Given a key, fetches the associated BinTrie using it's data as an offset.
handle_call({get_gameinfo, GameName}, _From, State) ->
    {ok, GameInfo} = orddict:find(GameName, State),
    {reply, {ok, GameInfo}, State}.

%% We don't really need the rest of the OTP interface...
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_PreviousVersion, _State, _Extra) ->
    ok.

