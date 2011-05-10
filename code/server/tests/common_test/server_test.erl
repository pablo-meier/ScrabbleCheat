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

-module(server_test).
-include_lib("eunit/include/eunit.hrl").

-include("scrabbleCheat_thrift.hrl").


%% In most XUnit frameworks, calls to these or related functions are automated... 
%% bleh :(  Maybe a TODO to make these macros eventually, though I don't know much
%% about Erlang macros.
%%
%% In this set of tests, setup and teardown will start new instances of the server,
%% close the server, and create a new client to test calls with.
setup() ->
    Server = scrabblecheat_main:start_link(?PORT),
    Client = thrift_client_util:new(?LOCALHOST, ?PORT, scrabbleCheat_thrift, []),
    case Server of 
        {ok, ServerName} -> 
            case Client of 
                {ok, ClientName} -> {ok, ServerName, ClientName};
                _Else -> throw({unexpected_client_value, Client})
            end;
        _Else -> throw({unexpected_server_value, Server})
    end.


