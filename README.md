[![Build Status](https://travis-ci.org/ostinelli/cowbell.svg?branch=master)](https://travis-ci.org/ostinelli/cowbell)
[![Hex pm](https://img.shields.io/hexpm/v/cowbell.svg)](https://hex.pm/packages/cowbell)

# Cowbell
**Cowbell** is an Erlang node connection manager.


## Introduction
Erlang nodes do not automatically reconnect when a node gets disconnected. For instance, when net splits happen, the nodes in a cluster might get disconnected and action needs to be taken to reconnect them again.

Cowbell automatically manages the connections & reconnections for you.


## Install

### Rebar3
If you're using [rebar3](https://github.com/erlang/rebar3), add `cowbell` as a dependency in your project's `rebar.config` file:

```erlang
{cowbell, {git, "git://github.com/ostinelli/cowbell.git", {tag, "1.0.1"}}}
```

Or, if you're using [Hex.pm](https://hex.pm/) as package manager (with the [rebar3_hex](https://github.com/hexpm/rebar3_hex) plugin):

```erlang
{cowbell, "1.0.1"}
```

Then, compile:

```bash
$ rebar3 compile
```

## Usage

### Setup
Ensure that Cowbell is started from your application. This can be done by either providing it as a dependency in your `.app` file, or by starting it manually:

```erlang
cowbell:start().
```

### Specify Nodes & Options
Nodes and options can be set in the environment variable `cowbell`. You're probably best off using an application configuration file (in releases, `sys.config`):

```erlang
{cowbell, [

    %% check interval for nodes events (Default: 10)
    %% {check_interval_sec, 10},

    %% abandon a node after unsuccessful reconnect (Default: 86400)
    %% {abandon_node_after_sec, 86400},

    %% list of nodes to connect to and monitor
    {nodes, [
        %% 'cowbell@127.0.0.1'
    ]}

]}.
```

Your application will have its own logic on when to connect to the other nodes in the cluster. To connect to the nodes and start monitoring for disconnections, issue:

```erlang
cowbell:connect_nodes().
```
A possible place to do so is in the `start/2` function in your main application module, something along the lines of:

```erlang
-module(myapp_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% connect to nodes
    cowbell:connect_nodes(),
    %% start sup
    myapp_sup:start_link().
```

Cowbell will then start monitoring nodes.

> You may prefer to initialize Cowbell inside of the root supervisor instead. This is particularly true if you are using OTP's `included_applications` feature.

## Why "Cowbell"?
_"A cow bell or cowbell is a bell worn by freely roaming livestock, making animals easier to locate should they wander off."_
- From [Wikipedia](https://en.wikipedia.org/wiki/Cowbell).


## Contributing
So you want to contribute? That's great! Please follow the guidelines below. It will make it easier to get merged in.

Before implementing a new feature, please submit a ticket to discuss what you intend to do. Your feature might already be in the works, or an alternative implementation might have already been discussed.

Do not commit to master in your fork. Provide a clean branch without merge commits. Every pull request should have its own topic branch. In this way, every additional adjustments to the original pull request might be done easily, and squashed with `git rebase -i`. The updated branch will be visible in the same pull request, so there will be no need to open new pull requests when there are changes to be applied.

Ensure that you include proper testing. To run tests you simply have to be in the project's root directory and run:

```bash
$ make tests
```
