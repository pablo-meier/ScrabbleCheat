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

-module(bin_trie).

%% GADDAG API
-export([is_key/2,
         erase/2,
         find/2,
         fetch_keys/1,
         get_root/1,
         is_terminator/1]).


-behaviour(gen_server).

%% gen_server API
-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% external API
-export([start_link/0,
         start_link_from_file/1,
         start_from_file/1,
         start/0]).

%% What we pass around are a binary tag, and the name of the game.
-record(ticket, {bin, name}).


%% The motivation for this module is a response to a number of memory-related
%% problems the previous tries (build on top of gb_trees) used in the gaddag
%% module was giving us. Namely:
%%
%%   * Standard Erlang terms are GC-ed very differently than binaries: large 
%%     binaries get allocated to a separate space than most process heaps, and
%%     don't get copied generationally. This means potentially large binaries
%%     aren't in the stop-and-copy, usually small since it splits your memory
%%     in half.
%%   * The standard Erlang terms were a fair bit bigger too: as a binary on
%%     disk, the GADDAGs were 260-300 MB; expanded, they got to be larger.
%%   * This led to us running out of memory, especially when the data 
%%     structure was getting modified or copied without us knowing (see below)
%%   * It's fun to hack on data structures and binaries ^_^
%%
%% The result of these factors was the VM trying to allocate ~700MB at a time
%% when performing a play_move verification! It's unclear whether this was due
%% to a GC in a copy phase of a stop-and-copy collection (many crashes occurred
%% with the most memory-hungry state "Garbing"), or in response to pulling a 
%% value from a data structure, and it prematurely copied rather than being 
%% understood to be read-only. This was compounded when I saw/remembered that 
%% the movesearch algo was deleting branches of GADDAGs when creating 
%% followstructs, which nearly guaranteed a copy.
%%
%% The solution, instead, becomes a pure-binary implementation with stronger
%% invariants than the general gb_trees provides. Binaries are GC-ed much more
%% conservatively, and when they get large enough, are not copied until
%% absolutely necessary. This would also be considerably smaller.
%%
%% Another advantage to this is the Trie is "pointer" based, in a sense: we use
%% absolute offsets in byte sizes, meaning any Trie node can be passed/edited
%% independently of the rest of the data structure.  So all you _really_ need
%% to pass from place to place are individual nodes of the trie; after 
%% construction you can just store the whole thing read-only from a gen_server.
%%
%% This pure-binary implementation of a Trie, will have the following 
%% restrictions:
%%
%%  * Each node begins with an 8-bit byte representing the number of branches
%%    this node contains.  
%%  * The next N 40-bit blocks are an alphabetical listing of that node, with
%%    the first 8 bits describing the character, and the next 32 bits pointing
%%    to that character's trie node as an absolute offset in the main data 
%%    structure.
%%  * The after this, we have an 8-bit byte that is either uint8_t MAX_VALUE
%%    or 0, representing whether this node is a terminator or not.
%%
%% Note that this module has both the gen_server that keeps the global state,
%% and the bin_trie methods to access its data.


-type bintrie() :: {binary(), atom()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRIE INTERFACE

%% Returns true if the BinTrie points to a branch at the keyed value.
-spec is_key(char(), bintrie()) -> boolean().
is_key(Key, #ticket{bin = BinTrie, name = _DictName}) ->
    case is_valid_key(Key) of
        true ->
            Keys = keys(BinTrie),
            lists:any(fun ({K,_}) -> K =:= Key end, Keys);
        false ->
            throw({out_of_trie_range, Key})
    end.
    

%% Returns a version of this bintrie() with the key 'erased,' meaning we
%% null out the values to zero.
-spec erase(char(), bintrie()) -> bintrie().
erase(Key, #ticket{bin = BinTrie, name = DictName}) ->
    case is_valid_key(Key) of
        true ->
            Without = lists:filter(fun ({K,_}) -> K =/= Key end, keys(BinTrie)),
            AsBinKeys = list_to_binary_keys(Without),

            %% we subtract by 1 because we just erased a key...
            Size = nodesize_bin(BinTrie) - 1, 
            IsTerminator = is_terminator_bin(BinTrie),
            NewBin = <<Size:8/little-unsigned-integer, 
                       AsBinKeys/binary, 
                       IsTerminator:8/little-unsigned-integer>>,
            {ticket, NewBin, DictName};
        false ->
            throw({out_of_trie_range, Key})
    end.


%% Given a key and a bintrie, returns the link if it has one.
-spec find(char(), bintrie()) -> {ok, bintrie()} | none.
find(Key, #ticket{bin = BinTrie, name = DictName}) ->
    Keys = keys(BinTrie),
    Ret = lists:filter(fun ({K,_}) -> K =:= Key end, Keys),
    case Ret of
        [] -> {branch_not_found, Key};
        [{Key, Offset}] ->
            NewBin = from_master_offset(Offset, DictName),
            {ticket, NewBin, DictName};
        Else ->
            throw({multiple_instances_of_key, Key, Else})
    end.


%% Fetches a list of keys the Bintrie has.
-spec fetch_keys(bintrie()) -> list(char()).
fetch_keys(#ticket{bin = BinTrie, name = _DictName}) ->
    Keys = keys(BinTrie),
    lists:map(fun ({K,_}) -> K end, Keys).

%% Fetches the root node.
-spec get_root(atom()) -> binary().
get_root(DictName) ->
    Binary = from_master_offset(0, DictName),
    {ticket, Binary, DictName}.


-spec is_terminator(bintrie()) -> boolean().
is_terminator(#ticket{bin = BinTrie, name = _DictName}) ->
    Value = is_terminator_bin(BinTrie),
    Value =/= 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STARTING/STOPPING 

make_state() ->
    Games = [twl06, sowpods, zynga],
    Storage = orddict:new(),

    lists:foldl( fun (DictName, CurrStorage) ->
               
                     %%DictFile = lists:concat([code:priv_dir(scrabblecheat), DictName, ".dict"]),
                     DictFile = lists:concat(["priv/", DictName, ".dict"]),
                     {ok, Gaddag} = file:read_file(DictFile),
                     orddict:store(DictName, Gaddag, CurrStorage)

                 end, Storage, Games).


start_link() ->
    State = make_state(),
    gen_server:start_link({local, giant_bintrie}, ?MODULE, State, []).

start() ->
    State = make_state(),
    gen_server:start({local, giant_bintrie}, ?MODULE, State, []).


%% These methods allow one to specify a filename from which to load, rather than
%% doing it dynamically with code. This is only really useful for unit testing, 
%% since code:priv_dir doesn't resolve unless you package a release.
start_link_from_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    State = orddict:store(twl06, Binary, orddict:new()),
    gen_server:start_link({local, giant_bintrie}, ?MODULE, State, []).

start_from_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    State = orddict:store(twl06, Binary, orddict:new()),
    gen_server:start({local, giant_bintrie}, ?MODULE, State, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER BEHAVIOR

%% init :: [Args] -> {ok, State} | {stop, Reason}
init(Args) ->
    {ok, Args}.

%% fetch :: char() * bintrie() -> bintrie()
%%
%% Given a key, fetches the associated BinTrie using it's data as an offset.
handle_call({fetch, Offset, DictName}, _From, State) ->
    {ok, Gaddag} = orddict:find(DictName, State),
    Size = binary:at(Gaddag, Offset),
    Return = binary:part(Gaddag, {Offset, (Size * 5) + 2}),
    {reply, {ok, Return}, State}.

%% We don't really need the rest of the OTP interface...
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_PreviousVersion, _State, _Extra) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS

-spec from_master_offset(number(), atom()) -> bintrie().
from_master_offset(Offset, DictName) ->
    {ok, Node} = gen_server:call(giant_bintrie, {fetch, Offset, DictName}),
    Node.


nodesize(BinTrie) ->
    binary:first(BinTrie).

keys(BinTrie) ->
    Size = nodesize(BinTrie),
    KeyBloc = binary:part(BinTrie, {1, Size * 5}),
    binary_keys_to_list(KeyBloc, []).

binary_keys_to_list(<<>>, Accum) -> lists:reverse(Accum);
binary_keys_to_list(<<Key:8/little-unsigned, Val:32/little-unsigned-integer, Rst/binary>>, Accum) ->
    Rslt = {Key, Val},
    binary_keys_to_list(Rst, [Rslt|Accum]).

list_to_binary_keys(Lst) -> 
	list_to_binary_keys(Lst, <<>>).
list_to_binary_keys([], Bin) -> Bin;
list_to_binary_keys([{K,V}|T], Bin) -> 
    BinKey = binary:encode_unsigned(K, little),
    BinVal = extend_to_32_bits(binary:encode_unsigned(V, little)),
    list_to_binary_keys(T, <<Bin/binary, BinKey/binary, BinVal/binary>>).

extend_to_32_bits(Bin) when byte_size(Bin) == 4 ->
    Bin;
extend_to_32_bits(Bin) when byte_size(Bin) < 4 ->
    extend_to_32_bits(<<Bin/binary, 0>>);
extend_to_32_bits(Bin) ->
    throw({extending_too_large_value, Bin}).



nodesize_bin(BinTrie) ->
    binary:at(BinTrie, 0).

is_terminator_bin(BinTrie) ->
    Size = nodesize(BinTrie),
    Zoom = (Size * 5) + 1,
    binary:at(BinTrie, Zoom).
  
is_valid_key(Key) ->
    (Key == $&) orelse (Key >= $A andalso Key =< $Z).
