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

-module(gaddag_looper).

-behaviour(gen_server).

-define(DICT_PATH, "priv/dicts/").
-define(DICT_OUT_PATH, "priv/").

-define(TEST_DICT_FILE, "../tests/eunit/testdict.txt").

%% gen_server API
-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).


%% external API
-export([start_link/0]).

%% Module with logic for the process that stores operations on the GADDAG.  
%% Before we managed all things to do with the GADDAG (board/gamestate/move
%% correctness verification, creation of word functions) in ETS tables, but
%% this proved troublesome, since large binaries are copied into the 
%% processor

%% State is a simple orddict that with keys sowpods, twl06, or zynga.  The 
%% values are the GADDAGs.


start_link() ->
    gen_server:start_link({local, gaddag_looper}, 
                          ?MODULE, 
                          [], 
                          [{spawn_opt, [{fullsweep_after, 0}]}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  GEN_SERVER FUNCTIONS

%% init :: [Args] -> {ok, State} | {stop, Reason}
%%
%% Establishes the orddict of Gaddags as it's state. Most of the real work is 
%% in start_link.
init(_Args) ->
    try 
        State = lists:foldl(fun (GameName, Orddict) ->
                                G = get_or_make_gaddag(GameName),
                                io:format(user, "entering ~p in ~p~n", [GameName, orddict:fetch_keys(Orddict)]),
                                orddict:store(GameName, G, Orddict)
                            end, orddict:new(), [twl06, sowpods, zynga]), 
        io:format(user, "Keys in State from start_link is ~p~n", [orddict:fetch_keys(State)]),
        {ok, State}
    catch
        _:_ -> {stop, "Couldn't load GADDAGS from files"} 
    end.


%% handle_call :: Request * From * State -> reply()
%%
%% Handles syncronous calls that require a response. The bulk of the behaviour.
handle_call({verify_gamestate, GS}, _From, State) ->
    try 
        io:format(user, "Handle call received~n", []),
        Board = gamestate:get_gamestate_board(GS),
        Dict = gamestate:get_gamestate_dict(GS),
        io:format(user, "Got gamestate data~n", []),
        io:format(user, "Retrieving Gaddag: ~p in ~p~n", [Dict, orddict:fetch_keys(State)]),
        Gaddag = orddict:find(Dict, State),
        io:format(user, "calling board:verify~n", []),
        board:verify(Board, Gaddag),
        {reply, gamestate_ok, State}
    catch
        throw:{_,_} -> {reply, {error, "The gamestate has a bad board"}, State}
    end;


handle_call({verify_move, Move, Gamestate}, _From, State) ->
    io:format(user, "State is ~p~n", [orddict:fetch_keys(State)]),
    Tiles = move:get_move_tiles(Move),
    Board = gamestate:get_gamestate_board(Gamestate),
    Dict = gamestate:get_gamestate_dict(Gamestate),
    io:format(user, "Looking for ~p~n", [Dict]),
    case Tiles of
        [] -> {reply, {error, "The move is empty!"}, State};
        _Else ->
            WithMove = board:place_move_on_board(Move, Board),
            try
                Gaddag = orddict:find(Dict, State),
                board:verify(WithMove, Gaddag),
                {reply, move_ok, State}
            catch
                {badArgsException, _} -> 
                    {reply, {error, "Not a valid move for this board"}, State}
            end
    end;


handle_call({verify_board, Board, Dict}, _From, State) ->
    Gaddag = orddict:find(Dict, State),
    try
        board:verify(Board, Gaddag),
        {reply, board_ok, State}
    catch
        throw:_ -> {reply, {error, "Not a valid board"}, State}
    end;


handle_call({search_for_moves, Board, Rack, Dict}, _From, State) ->
    Gaddag = orddict:find(Dict, State),
    Moves = movesearch:get_all_moves(Board, Rack, Gaddag),

    %% TODO: sending back the list of moves in its entirety is mayhaps a bit much for the
    %% 'small messages, large computations' idea.  If we could minimize the list of moves
    %% to send out (rather than how we brutishly send all of them as we currently do), all
    %% the better.
    {reply, {moves, Moves}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, "Message sent to gaddag_looper that is misunderstood."}, State}.


%% handle_cast :: 
%%
%% Handles asyncrounous calls. While part of the OTP interface, we don't really
%% need these.
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


%% terminate :: atom() * State -> ()
%%
%% The opposite of init, clean yer business here.
terminate(_Reason, _State) ->
    ok.


%% code_change :: 
code_change(_PreviousVersion, _State, _Extra) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  HELPERS


%% get_or_make_gaddag :: () -> Gaddag
%%
%% Searches a few predefined paths for files to produce a dictionary Gaddag at
%% boot time.  Either finds a predefined binary one, or produces one.
get_or_make_gaddag(GameName) ->
    PrivDir = code:priv_dir(scrabblecheat),
    AsStr = atom_to_list(GameName),
    Path = string:concat(PrivDir, "/" ++ AsStr ++ ".gaddag"),
    case file:read_file_info(Path) of
        {ok, _} -> 
            io:format("Reading a dictionary, this may take a few seconds...~n"),
            dict_parser:read_from_binary(Path);
        {error, _} -> 
            io:format("Gaddag file not found!  Using test dictionary in meantime.~n"),
            io:format("run `make binary-gaddag` to generate the full dictionary.~n"),
            case file:read_file_info(?TEST_DICT_FILE) of
                {ok, _} -> dict_parser:parse(?TEST_DICT_FILE);
                {error, _} -> dict_parser:parse("test/testdict.txt")
            end
        end.



