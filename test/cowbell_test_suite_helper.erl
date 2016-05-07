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
-module(cowbell_test_suite_helper).

%% API
-export([set_environment_variables/0, set_environment_variables/1]).
-export([start_slave/1, stop_slave/1]).
-export([connect_node/1, disconnect_node/1]).

%% macros
-define(COWBELL_TEST_CONFIG_FILENAME, "cowbell-test.config").


%% ===================================================================
%% API
%% ===================================================================
set_environment_variables() ->
    set_environment_variables(node()).
set_environment_variables(Node) ->
    % read config file
    ConfigFilePath = filename:join([filename:dirname(code:which(?MODULE)), ?COWBELL_TEST_CONFIG_FILENAME]),
    {ok, [AppsConfig]} = file:consult(ConfigFilePath),
    % loop to set variables
    F = fun({AppName, AppConfig}) ->
        set_environment_for_app(Node, AppName, AppConfig)
    end,
    lists:foreach(F, AppsConfig).

start_slave(NodeShortName) ->
    CodePath = code:get_path(),
    {ok, Node} = ct_slave:start(NodeShortName, [{boot_timeout, 10}]),
    true = rpc:call(Node, code, set_path, [CodePath]),
    {ok, Node}.

stop_slave(NodeShortName) ->
    {ok, _} = ct_slave:stop(NodeShortName).

connect_node(Node) ->
    net_kernel:connect_node(Node).

disconnect_node(Node) ->
    erlang:disconnect_node(Node).

%% ===================================================================
%% Internal
%% ===================================================================
set_environment_for_app(Node, AppName, AppConfig) ->
    F = fun({Key, Val}) ->
        ok = rpc:call(Node, application, set_env, [AppName, Key, Val])
    end,
    lists:foreach(F, AppConfig).
