-module(replication).

-export([start/1, stop/0]).
-export([cluster/0, join_cluster/1, leave_cluster/0, whois_Master/0, replicate_from/0, replicate_from/1]).

-include("node_records.hrl").

start(ShortName) ->
	net_kernel:start([ShortName, shortnames]),

	% starting mnesia
	mnesia:create_schema(node()),
	mnesia:start(),
	mnesia:create_table(ldb_nodes, [{type, set},
									   {ram_copies,[node()]},
									   {local_content, true},
									   {attributes, record_info(fields, ldb_nodes)}]).


stop() ->
	mnesia:stop().

cluster() ->
	AvailableNodes = [node(), nodes()],
	% TODO: check if available nodes are up
	% update if not
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{_, {MasterNode, [SlaveNodes]}} = Data;
		_ ->
			{not_connected, {}}
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
			{_, {MasterNode, [SlaveNodes]}} = Data,
			% TODO: remove node() from list
			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=SlaveNodes}),
			{left_cluster, {}};
		_ ->
			{not_connected, {}}
	end.

whois_Master() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{_ , {MasterNode, _ }} = Data,
		    {ok, MasterNode};
		_ ->
			{not_connected, {}}
	end.

replicate_from() ->
	replicate_from(whois_Master()).

replicate_from(Node) ->
	ok.
