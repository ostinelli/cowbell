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

%% macros
-define(DEFAULT_CHECK_INTERVAL_SEC, 10).
-define(DEFAULT_ABANDON_NODE_AFTER_SEC, 86400).

%% types
-type disconnected_node_info() :: {Node :: atom(), DisconnectedAt :: non_neg_integer()}.

%% records
-record(state, {
    monitored_nodes = [] :: [atom()],
    disconnected_nodes_info = [] :: list(),
    check_interval_sec = 0 :: non_neg_integer(),
    abandon_node_after_sec :: non_neg_integer(),
    timer_ref = undefined :: undefined | reference()
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
    CheckIntervalSec = application:get_env(cowbell, check_interval_sec, ?DEFAULT_CHECK_INTERVAL_SEC),
    AbandonNodeAfterSec = application:get_env(cowbell, abandon_node_after_sec, ?DEFAULT_ABANDON_NODE_AFTER_SEC),
    MonitoredNodes = application:get_env(cowbell, nodes, []),

    %% prepare nodes info
    DisconnectedNodesInfo = init_disconnected_node_info(MonitoredNodes),

    %% build state
    {ok, #state{
        monitored_nodes = MonitoredNodes,
        disconnected_nodes_info = DisconnectedNodesInfo,
        check_interval_sec = CheckIntervalSec,
        abandon_node_after_sec = AbandonNodeAfterSec
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

handle_call(connect_nodes, _From, State) ->
    %% start listening for events
    ok = net_kernel:monitor_nodes(true),
    %% connect
    State1 = connect_nodes(State),
    %% return
    {reply, ok, State1};

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
    monitored_nodes = MonitoredNodes,
    disconnected_nodes_info = DisconnectedNodesInfo,
    check_interval_sec = CheckIntervalSec,
    abandon_node_after_sec = AbandonNodeAfterSec
} = State) ->
    case lists:member(Node, MonitoredNodes) of
        true ->
            error_logger:warning_msg(
                "Node '~p' got disconnected, will try reconnecting every ~p seconds for a max of ~p seconds",
                [Node, CheckIntervalSec, AbandonNodeAfterSec]
            ),
            %% add to disconnected nodes
            DisconnectedAt = epoch_time(),
            State1 = State#state{disconnected_nodes_info = [{Node, DisconnectedAt} | DisconnectedNodesInfo]},
            %% reply
            {noreply, State1};
        false ->
            error_logger:warning_msg("Node '~p' got disconnected but not in list of monitored nodes, ignoring", [Node]),
            %% reply
            {noreply, State}
    end;

handle_info({nodeup, Node}, #state{
    disconnected_nodes_info = DisconnectedNodesInfo
} = State) ->
    error_logger:info_msg("Node '~p' got connected", [Node]),
    %% remove from list
    DisconnectedNodesInfo1 = lists:keydelete(Node, 1, DisconnectedNodesInfo),
    %% return
    {noreply, State#state{disconnected_nodes_info = DisconnectedNodesInfo1}};

handle_info(connect_nodes, State) ->
    %% connect
    State1 = connect_nodes(State),
    %% return
    {noreply, State1};

handle_info(Info, State) ->
    error_logger:warning_msg("Received an unknown info message: ~p", [Info]),
    {noreply, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Terminate
%% ----------------------------------------------------------------------------------------------------------
-spec terminate(Reason :: any(), #state{}) -> terminated.
terminate(Reason, _State) ->
    error_logger:info_msg("Terminating cowbell monitor with reason: ~p", [Reason]),
    %% return
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
-spec init_disconnected_node_info(Nodes :: [atom()]) -> [disconnected_node_info()].
init_disconnected_node_info(Nodes) ->
    DisconnectedAt = epoch_time(),
    F = fun(Node) -> {Node, DisconnectedAt} end,
    lists:map(F, Nodes).

-spec connect_nodes(#state{}) -> #state{}.
connect_nodes(#state{
    disconnected_nodes_info = DisconnectedNodesInfo,
    abandon_node_after_sec = AbandonNodeAfterSec
} = State) ->
    %% run
    DisconnectedNodesInfo1 = connect_nodes(DisconnectedNodesInfo, AbandonNodeAfterSec),
    %% add timeout
    State1 = timeout(State),
    %% return
    State1#state{disconnected_nodes_info = DisconnectedNodesInfo1}.

-spec connect_nodes([disconnected_node_info()], AbandonNodeAfterSec :: non_neg_integer()) ->
    [disconnected_node_info()].
connect_nodes(DisconnectedNodesInfo, AbandonNodeAfterSec) ->
    connect_nodes(DisconnectedNodesInfo, AbandonNodeAfterSec, []).

-spec connect_nodes(
    [disconnected_node_info()],
    AbandonNodeAfterSec :: non_neg_integer(),
    AccNodesInfo :: [disconnected_node_info()]
) -> [disconnected_node_info()].
connect_nodes([], _, AccNodesInfo) -> AccNodesInfo;
connect_nodes([{Node, DisconnectedAt} = NodeInfo | TNodesInfo], AbandonNodeAfterSec, AccNodesInfo) ->
    %% get current time
    CurrentTime = epoch_time(),
    %% acc
    AccNodesInfo1 = case CurrentTime - DisconnectedAt >= AbandonNodeAfterSec of
        true ->
            %% abandon node
            error_logger:info_msg(
                "Could not connect to node '~p' after retrying for ~p seconds, abandoning node",
                [Node, CurrentTime - DisconnectedAt]
            ),
            %% abandoned, do not add to acc
            AccNodesInfo;

        _ ->
            %% try to connect
            case net_kernel:connect_node(Node) of
                true ->
                    %% successful, do not add to acc
                    AccNodesInfo;
                _ ->
                    %% unsuccessful, add to acc for next round
                    [NodeInfo | AccNodesInfo]
            end
    end,
    connect_nodes(TNodesInfo, AbandonNodeAfterSec, AccNodesInfo1).

-spec epoch_time() -> non_neg_integer().
epoch_time() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

-spec timeout(#state{}) -> #state{}.
timeout(#state{
    check_interval_sec = CheckIntervalSec,
    timer_ref = TimerPrevRef
} = State) ->
    case TimerPrevRef of
        undefined -> ignore;
        _ -> erlang:cancel_timer(TimerPrevRef)
    end,
    TimerRef = erlang:send_after(CheckIntervalSec * 1000, self(), connect_nodes),
    State#state{timer_ref = TimerRef}.
