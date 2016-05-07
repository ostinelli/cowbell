%% ==========================================================================================================
%% Cowbell -A node connection manager that handles reconnections in dynamic environments.
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2016 Roberto Ostinelli <roberto@ostinelli.net>.
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
%% ==========================================================================================================
-module(cowbell_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([connect_nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% internal
-export([reconnect_node_loop/2]).

%% macros
-define(DEFAULT_RETRY_INTERVAL_MS, 10000).

%% records
-record(state, {
    nodes = [] :: list(),
    retry_interval_ms = 0 :: non_neg_integer()
}).

%% ===================================================================
%% API
%% ===================================================================
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    Options = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Options).

-spec connect_nodes() -> ok.
connect_nodes() ->
    gen_server:call(?MODULE, connect_nodes).

%% ===================================================================
%% Callbacks
%% ===================================================================

%% ----------------------------------------------------------------------------------------------------------
%% Init
%% ----------------------------------------------------------------------------------------------------------
-spec init([]) ->
    {ok, #state{}} |
    {ok, #state{}, Timeout :: non_neg_integer()} |
    ignore |
    {stop, Reason :: any()}.
init([]) ->
    %% get preferences
    RetryIntervalMs = application:get_env(cowbell, retry_interval_ms, ?DEFAULT_RETRY_INTERVAL_MS),
    {ok, Nodes} = application:get_env(nodes),

    %% listen for events
    ok = net_kernel:monitor_nodes(true),

    %% build state
    {ok, #state{
        nodes = Nodes,
        retry_interval_ms = RetryIntervalMs
    }}.

%% ----------------------------------------------------------------------------------------------------------
%% Call messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_call(Request :: any(), From :: any(), #state{}) ->
    {reply, Reply :: any(), #state{}} |
    {reply, Reply :: any(), #state{}, Timeout :: non_neg_integer()} |
    {noreply, #state{}} |
    {noreply, #state{}, Timeout :: non_neg_integer()} |
    {stop, Reason :: any(), Reply :: any(), #state{}} |
    {stop, Reason :: any(), #state{}}.

handle_call(connect_nodes, _From, #state{
    nodes = Nodes
} = State) ->
    connect_nodes(Nodes),
    {reply, ok, State};

handle_call(Request, From, State) ->
    error_logger:warning_msg("Received from ~p an unknown call message: ~p", [Request, From]),
    {reply, undefined, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Cast messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_cast(Msg :: any(), #state{}) ->
    {noreply, #state{}} |
    {noreply, #state{}, Timeout :: non_neg_integer()} |
    {stop, Reason :: any(), #state{}}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("Received an unknown cast message: ~p", [Msg]),
    {noreply, State}.

%% ----------------------------------------------------------------------------------------------------------
%% All non Call / Cast messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_info(Info :: any(), #state{}) ->
    {noreply, #state{}} |
    {noreply, #state{}, Timeout :: non_neg_integer()} |
    {stop, Reason :: any(), #state{}}.

handle_info({nodedown, Node}, #state{
    retry_interval_ms = RetryIntervalMs
} = State) ->
    error_logger:warning_msg("Node ~p got disconnected, will try to reconnect in ~p ms", [Node, RetryIntervalMs]),
    spawn_link(?MODULE, reconnect_node_loop, [Node, RetryIntervalMs]),
    {noreply, State};

handle_info({nodeup, Node}, State) ->
    error_logger:warning_msg("Node ~p got connected", [Node]),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:warning_msg("Received an unknown info message: ~p", [Info]),
    {noreply, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Terminate
%% ----------------------------------------------------------------------------------------------------------
-spec terminate(Reason :: any(), #state{}) -> terminated.
terminate(Reason, _State) ->
    error_logger:info_msg("Terminating cowbell monitor with reason: ~p", [Reason]),
    terminated.

%% ----------------------------------------------------------------------------------------------------------
%% Convert process state when code is changed.
%% ----------------------------------------------------------------------------------------------------------
-spec code_change(OldVsn :: any(), #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================
-spec connect_nodes(Nodes :: [atom()]) -> [atom()].
connect_nodes(Nodes) ->
    [i_connect_node(Node) || Node <- Nodes].

-spec reconnect_node_loop(Node :: atom(), RetryIntervalMs :: non_neg_integer()) -> ok.
reconnect_node_loop(Node, RetryIntervalMs) ->
    timer:sleep(RetryIntervalMs),
    case i_connect_node(Node) of
        true -> ok;
        false -> reconnect_node_loop(Node, RetryIntervalMs)
    end.

-spec i_connect_node(Node :: atom()) -> boolean().
i_connect_node(Node) ->
    case net_kernel:connect_node(Node) of
        true ->
            true;
        _ ->
            error_logger:info_msg("Could not connect to node '~p' yet"),
            false
    end.
