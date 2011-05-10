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

-module(server_SUITE).
-include_lib("common_test/include/ct.hrl").
%-include("ct.hrl").

%% Test server callbacks.
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         suite/0]).

%% Test cases
-export([new_game_test/1]).

all() ->
    [new_game_test].


suite() ->
    [{userdata, [{info, "Tests the top-level API, defined by the Thrift file."}]}].

init_per_suite(Config) ->
    application:start(scrabblecheat),
    Config.


end_per_suite(Config) ->
    %% Kill it, fo' sho
    application:stop(scrabblecheat),
    Config.

