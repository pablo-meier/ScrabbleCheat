-module('scrabblecheat_sup').
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Children = [
                {scrabblecheat_main, {scrabblecheat_main, start_link, []},
                permanent, 2000, worker, [scrabblecheat_main]},
                {gameinfo, {gameinfo, start_link, []},
                permanent, 2000, worker, [gameinfo]},
                {bin_trie, {bin_trie, start_link, []},
                permanent, 2000, worker, [bin_trie]}
               ],
    RestartStrategy = {one_for_all, 0, 1},
    {ok, {RestartStrategy, Children}}.
