%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-record(node_info, {node_name :: node(),
        replication_time ::  erlang:timestamp()}).

-record(ldb_nodes, {clusterID :: integer(),
        master_node ::node(),
        slave_nodes :: [node()],
        observer_nodes :: [node()]}).



