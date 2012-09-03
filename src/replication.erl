-module(replication).

-export([start/2, start/3, stop/0]).
-export([cluster/0, join_cluster/1, leave_cluster/0, whois_Master/0, replicate/1, make_Master/1, am_I_Master/1]).

-include("node_records.hrl").

% you need to start epmd first
% try running an erl -sname smth
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
			io:format("I AM NODE ~p ~n", [node()]),
			make_Master(node());
		Error ->
			{error, {failed_master, Error}}
	end.
  

start(ShortName, Cookie) ->
	net_kernel:start([ShortName, shortnames]),
	erlang:set_cookie(node(), Cookie),

	% starting mnesia for replication slave
	case mnesia:start() of
		ok ->
			{ok, slave};
		Error ->
			{error, {failed_slave, Error}}
	end.

stop() ->
	net_kernel:stop(),
	mnesia:stop().

cluster() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{ldb_nodes, _, MasterNode, SlaveNodes} = Data,
			{connected, [MasterNode, SlaveNodes]};
		Error ->
			{error, {not_connected, Error}}
	end.

%
% Join cluster as slave
%
join_cluster(ClusterName) ->
	case mnesia:change_config(extra_db_nodes, [ClusterName]) of
		{ok, [_]} -> % connected to cluster, lets read info
			case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
				[{ldb_nodes, _, MasterNode, SlaveNodes}] ->
					mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=sets:to_list(sets:from_list(SlaveNodes ++ [node()]))}),
					
					{connected, SlaveNodes};
				Error ->
					{error, {mnesia_error, Error}}
			end;
		{ok, []} ->
			{error, {not_connected, {}}};
		Error ->
			Error
	end.


leave_cluster() ->
	IsMaster = am_I_Master(node()),
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		_ when IsMaster =:= yes ->
			{error, {not_permitted, {}}};

		[Data] -> 
			{ldb_nodes, _, MasterNode, SlaveNodes} = Data,
			% TODO: remove node() from list
			
			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=SlaveNodes}),
			{disconnect_cluster, {}};
		Error ->
			{error, {not_connected, Error}}
	end.

whois_Master() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[{ldb_nodes, "ldbNodes", MasterNode, [_]}] -> 
		    {ok, MasterNode};
		Error ->
			{error, {not_connected, Error}}
	end.

am_I_Master(NodeName) ->
	case whois_Master() of
		{ok, NodeName} -> yes;
		{ok, _} -> no;
		Error ->
			{error, {not_connected, Error}}
	end.
	
make_Master(NodeName) ->
	IsMaster = am_I_Master(NodeName),
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		_ when IsMaster =:= yes ->
			{error, {already_master, {}}};

		%% node is the first master
		[] ->
			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NodeName, slave_nodes=[]}),
			{ok, i_am_master};

		%% if node is a slave
		%% remove him from there!
		[{ldb_nodes, _, MasterNode, SlaveNodes}] ->
			NodeIsSlave = (SlaveNodes =/= ([NodeName] -- SlaveNodes)),
			if NodeIsSlave =:= true  ->
					mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NodeName, 
												  slave_nodes= (SlaveNodes -- [NodeName]) ++ [MasterNode]}),
					{ok, i_am_master};
			   true ->
					{error, {already_master, {}}}
			end;
		Error ->
			{error, {not_connected, Error}}
	end.

% replication(whois_Master()).
replicate(Node) ->
	Node.
