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


%% MOAR OTP COMPLIANCE!
-module(scrabblecheat_sup).
-behaviour(supervisor).

%% Supervisor callbacks.
-export([init/1]).

-define(SERVER, ?MODULE).


%% API
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %Dict = code:priv_dir(scrabblecheat) ++ "/twl06.dict",
    Dict = "priv" ++ "/twl06.dict",

    Children = [{scrabblecheat_main, {scrabblecheat_main, start_link, []},
                permanent, 2000, worker, [scrabblecheat_main]},
                {bin_trie, {bin_trie, start_link, []},
                permanent, 2000, worker, [bin_trie]}
                ],
    RestartStrategy = {one_for_all, 0, 1},
    {ok, {RestartStrategy, Children}}.
