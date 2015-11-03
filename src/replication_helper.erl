%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(replication_helper).
-author("Jon Vlachoyiannis").

-export([otp_release/0, start_node/2]).

-include("replication.hrl").

-spec otp_release() -> integer().
otp_release() ->
    try
        erlang:list_to_integer(erlang:system_info(otp_release))
    catch
        error:badarg ->
            16
    end.

-spec start_node(atom(), atom()) -> {ok, {atom(), atom()}} | {error, {atom(), atom()}}.
start_node(Node, Cookie) when is_atom(Node), is_atom(Cookie)->
    %% Try to spin up net_kernel
    {ok, {Node, _}} = 
    case net_kernel:start([Node, longnames]) of
        {ok, _} ->
            {ok, {Node, started}};
        {error,{already_started, _Pid}} ->
            {ok, {Node, already_started}};
        {error,{{already_started, _Pid}}, _} ->
            {ok, {Node, already_started}};
        {error, Reason} ->
            ok = ?CONSOLE("function=start_node event=fail_start_node Reason=\"~p\"", [Reason]),
            {error, Reason}
    end,
    true = erlang:set_cookie(node(), Cookie).        
