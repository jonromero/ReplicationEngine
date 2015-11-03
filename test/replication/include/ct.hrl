%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

%%% Common Test includes
-include_lib("common_test/include/ct.hrl").
-define(APP, 'replication').

%%% Node definitions
-define(NODE, 'db_master@127.0.0.1').
-define(COOKIE, 'replication').
-define(SLAVE1, 'db_slave1@127.0.0.1').
-define(SLAVE2, 'db_slave2@127.0.0.1').
-define(OBSERVER, 'db_observer@127.0.0.1').
-define(FAKE_NODE, 'fake_node@127.0.1.1').
-define(SLAVE_IP, '127.0.0.1').
-define(SLAVE1_NAME, 'db_slave1').
-define(SLAVE2_NAME, 'db_slave2').
-define(OBSERVER_NAME, 'db_observer').

-define(restart_application(),
    begin
        ok = application:stop(?APP),
        ok = application:unload(?APP),
        ok = application:start(?APP)
    end
).
