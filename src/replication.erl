-module(replication).

-export([start/2, start/3, stop/0]).
-export([cluster/0, join_cluster/1, leave_cluster/0, whois_Master/0, replicate/0, replicate/1]).

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

%
% Join cluster as slave
%
join_cluster(ClusterName) ->
	% check if this node is already part of cluster
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[{ldb_nodes, _, MasterNode, SlaveNodes}] ->
			case lists:filter(fun(X) -> node() == X end, lists:flatten([MasterNode | SlaveNodes])) of
				[] -> % not in cluster, lets add the node
					case mnesia:change_config(extra_db_nodes, [ClusterName]) of
						{ok, [_]} -> 							
							mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", slave_nodes=[SlaveNodes|node()]}),
							{connected, [Nodes]};
						{ok, []} ->
							{not_connected, {}};
						Error ->
							Error
					end;
				_ -> 
				  {already_in, error}
			end;
		Error ->
			{not_connected, Error}
	end.


leave_cluster() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{ldb_nodes, _, MasterNode, SlaveNodes} = Data,
			% TODO: remove node() from list
			
			if am_I_Master() ->
					you_cannot_leave_the_cluster, a slave must Master you

			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=SlaveNodes}),
			{disconnect_cluster, {}};
		Error ->
			{not_connected, Error}
	end.

whois_Master() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[{ldb_nodes_, "ldbNodes", MasterNode, [SlaveNodes]}] -> 
		    {ok, MasterNode};
		Error ->
			{not_connected, Error}
	end.

am_I_Master() ->
	case whois_Master() of
		{ok, node()} -> yes;
		{ok, _} -> no;
		Error ->
			{not_connected, Error}
	end.
	
make_Master(NodeName) ->
	if not am_I_Master() ->

	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[{ldb_nodes, _, MasterNode, SlaveNodes}] -> 
			if node() in SlaveNodes ->
					mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NodeName, slave_nodes=SlaveNodes - NodeName}),
			{ok, i_am_master};
		Error ->
			{not_connected, Error}
	end.

replicate() ->
	replicate(whois_Master()).

replicate(Node) ->
	ok.
