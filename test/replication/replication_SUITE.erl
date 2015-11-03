%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(replication_SUITE).

-include_lib("test/replication/include/ct.hrl").

all() -> [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    test_helper:start_master(),
    test_helper:start_slave(?SLAVE1, ?SLAVE1_NAME),
    test_helper:start_slave(?SLAVE2, ?SLAVE2_NAME),
    test_helper:start_slave(?OBSERVER, ?OBSERVER_NAME),
    Config.

end_per_testcase(_, _Config) ->
    test_helper:stop(?SLAVE1),
    test_helper:stop(?SLAVE2),
    test_helper:stop(?OBSERVER),
    ok.





