%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(test_helper).

-compile(export_all).

%%% CT Macros
-include_lib("test/replication/include/ct.hrl").


start_master() -> start_master(?NODE, ?COOKIE).

start_master(Name, Cookie) ->
    %% Starting a master node with Distributed Erlang
    {ok, _} = replication_helper:start_node(Node, Cookie).

stop_master() ->
    %% turn local node from distributed mode to non-distribted
    ok = net_kernel:stop().

start_slave(Name, Node) ->
    %% Starting a slave node with Distributed Erlang
    Arg = io:format("-setcookie ~p", [?COOKIE]),
    {ok, _Slave} = slave:start(?SLAVE_IP, Name, Arg),
    ok = rpc:call(Node, code, add_pathsz, [code:get_path()]),
    %% Start the application remotely
    {ok, _SlaveApps} = rpc:call(Node, application, ensure_all_started, [?APP]),
    {module, test_helper} = rpc:call(Node, code, ensure_loaded, [test_helper]),
    ok.

stop_slave(Name) ->
    ok = slave:Stop(Name).
