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
%% -*- mode:erlang -*-
{application, scrabblecheat,
 [
  {description, "Server for the word game cheating software."},
  {vsn, "0.0.3"},
  {mod, {app_callback, []}},
  % All modules used by the application.
  {modules, [app_callback, dict_parser, gamestate, movesearch, thrift_helper,
             board, followstruct, main, string_utils, tile, board_parser, 
             gaddag, move, thrift_base64_transport, thrift_binary_protocol, 
             thrift_buffered_transport, thrift_client, thrift_client_util, 
             thrift_disk_log_transport, thrift_file_transport, 
             thrift_framed_transport, thrift_http_transport, 
             thrift_memory_buffer, thrift_processor, thrift_protocol, 
             thrift_server, thrift_service, thrift_socket_server, thrift_socket_transport, 
             thrift_transport, scrabbleCheat_thrift, scrabbleCheat_types, 
             thrift_transport_state_test]},

  % All of the registered names the application uses. This can be ignored.
  {registered, []},

  % Applications that are to be started prior to this one. This can be ignored
  % leave it alone unless you understand it well and let the .rel files in
  % your release handle this.
  {applications, [kernel, stdlib]}
 ]
}.
