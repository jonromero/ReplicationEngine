%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(test_helper).

-export([start_master/0, stop_master/0, start_slave/2, stop_slave/1]).

%%% CT Macros
-include_lib("test/replication/include/ct.hrl").

start_master() -> start_master(?NODE, ?COOKIE).

start_master(LongName, Cookie) ->
    %% Starting a master node with Distributed Erlang
    {ok, i_am_master} = replication:init(LongName, Cookie, master).

stop_master() ->
    %% turn local node from distributed mode to non-distribted
    {ok, disconnected_from_cluster} = replication:stop(normal),
    ok = net_kernel:stop().

start_slave(Name, Node) ->
    %% Starting a slave node with Distributed Erlang
    Arg = io:format("-setcookie ~p", [?COOKIE]),
    {ok, _Slave} = slave:start(?SLAVE_IP, Name, Arg),
    ok = rpc:call(Node, code, add_pathsz, [code:get_path()]),
    %% Start the application remotely
 %   {ok, _SlaveApps} = rpc:call(Node, application, ensure_all_started, [?APP]),
 %   {module, test_helper} = rpc:call(Node, code, ensure_loaded, [test_helper]),
    ok.

stop_slave(Name) ->
    ok = slave:stop(Name).
