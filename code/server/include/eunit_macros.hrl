%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%%
%% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson



%% Cribbing a few macros from EUnit for my Common Test testing, since 
%% there doesn't appear to be a way to check for failure as easily...

-define(assertException(Class, Term, Expr),
    ((fun () ->
        try (Expr) of
            __V -> .erlang:error({assertException_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {expression, (??Expr)},
                       {expected,
                    "{ "++(??Class)++" , "++(??Term)
                    ++" , [...] }"},
                       {unexpected_success, __V}]})
        catch
        Class:Term -> ok;
            __C:__T ->
            .erlang:error({assertException_failed,
                   [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected,
                     "{ "++(??Class)++" , "++(??Term)
                     ++" , [...] }"},
                    {unexpected_exception,
                     {__C, __T, erlang:get_stacktrace()}}]})
        end
      end)())).


-define(assert(BoolExpr),
    ((fun () ->
        case (BoolExpr) of
        true -> ok;
        __V -> .erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {expression, (??BoolExpr)},
                       {expected, true},
                       {value, case __V of false -> __V;
                           _ -> {not_a_boolean,__V}
                           end}]})
        end
      end)())).
