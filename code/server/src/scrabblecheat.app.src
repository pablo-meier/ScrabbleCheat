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
  {mod, {scrabblecheat_app, []}},
  % All modules used by the application.
  {modules, [scrabblecheat_app, scrabblecheat_sup, scrabblecheat_main, 
             dict_parser, gamestate, movesearch, thrift_helper, board, 
             followstruct, string_utils, tile, board_parser, gaddag, move,
             scrabbleCheat_thrift, scrabbleCheat_types]},

  % All of the registered names the application uses. This can be ignored.
  {registered, [scrabblecheat_sup]},

  % Applications that are to be started prior to this one. This can be ignored
  % leave it alone unless you understand it well and let the .rel files in
  % your release handle this.
  {applications, [kernel, stdlib]}
 ]
}.
