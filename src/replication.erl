-module(replication).

-export([start/2, start/3, stop/0]).
-export([cluster/0, join_cluster/1, leave_cluster/0, whois_Master/0, replicate_from/0, replicate_from/1]).

-include("node_records.hrl").

start(ShortName, Cookie, master) ->
	net_kernel:start([ShortName, shortnames]),
	erlang:set_cookie(node(), Cookie),

	% starting mnesia
	mnesia:create_schema(node()),
	mnesia:start(),
	case mnesia:create_table(ldb_nodes, [{type, set},
										 {ram_copies,[node()]},
										 {local_content, false},
										 {attributes, record_info(fields, ldb_nodes)}]) of
		{atomic, ok} ->
			ok;
		Error ->
			{failed_master, Error}
	end.
  

start(ShortName, Cookie) ->
	net_kernel:start([ShortName, shortnames]),
	erlang:set_cookie(node(), Cookie),

	% starting mnesia for replication slave
	case mnesia:start() of
		ok ->
			ok;
		Error ->
			{failed_slave, Error}
	end.

stop() ->
	net_kernel:stop(),
	mnesia:stop().

cluster() ->
	AvailableNodes = [node(), nodes()],
	% TODO: check if available nodes are up
	% update if not
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{ldb_nodes, _, MasterNode, SlaveNodes} = Data,
			{connected, [MasterNode, SlaveNodes]};
		Error ->
			{not_connected, Error}
	end.

join_cluster(ClusterName) ->
	case mnesia:change_config(extra_db_nodes, [ClusterName]) of
		{ok, [Nodes]} -> 
			% TODO: add node() to list
			MasterNode = "1",
			SlaveNodes = ["2","3","4"],

			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=SlaveNodes}),
			
			{connected, [Nodes]};
		{ok, []} ->
			{not_connected, {}};
		Error ->
			Error
	end.

leave_cluster() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{ldb_nodes, _, MasterNode, SlaveNodes} = Data,
			% TODO: remove node() from list
			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=SlaveNodes}),
			{disconnect_cluster, {}};
		Error ->
			{not_connected, Error}
	end.

whois_Master() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{ldb_nodes_, "ldbNodes", MasterNode, [SlaveNodes]} = Data,
		    {ok, MasterNode};
		Error ->
			{not_connected, Error}
	end.

replicate_from() ->
	replicate_from(whois_Master()).

replicate_from(Node) ->
	ok.
