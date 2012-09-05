-module(replication).

-export([start/2, start/3, stop/0]).
-export([refresh/0, join_cluster/1, leave_cluster/0, whois_Master/0, replicate/0, make_Master/1, am_I_Master/1, check_if_alive/1]).

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
										 {attributes, record_info(fields, ldb_nodes)}])  of
		{atomic, ok} ->
			mnesia:create_table(node_info, [{type, set},
											{ram_copies,[node()]},
											{local_content, false},
											{attributes, record_info(fields, node_info)}]),
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
			{ok, ready_to_join};
		Error ->
			{error, {failed_to_start, Error}}
	end.

stop() ->
	net_kernel:stop(),
	mnesia:stop().

% Returns a new Master based
% on who was the last the got replicated data
elections(LiveNodes) ->
	NodesInfo = lists:flatten(lists:map(fun(X) -> 
												mnesia:dirty_read(node_info, X) end, 
										LiveNodes)),
	
	io:format("XA ~p ~p ~n", [NodesInfo, LiveNodes]),
						  
	SortedNodes = lists:reverse(lists:map(fun(Y) ->
												  element(2, Y) end,
										  lists:keysort(3, NodesInfo))),
								
	io:format("Sorted nodes ~p ~n", [SortedNodes]),
	SortedNodes.
	
%
% Returns the nodes that are active,
% cleaning the table of dead nodes
% and elects a new Master if there is no Master
%
refresh() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[Data] -> 
			{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes} = Data,

			% remove slave nodes that are down
			SlaveNodesUp = lists:filter(fun(X) -> 
									 check_if_alive(X) end,
							 SlaveNodes),

			% remove observer nodes that are down
			ObserverNodesUp = lists:filter(fun(X) -> 
												   check_if_alive(X) end,
										   ObserverNodes),

			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, 
										  slave_nodes=sets:to_list(sets:from_list(SlaveNodesUp)),
										  observer_nodes=sets:to_list(sets:from_list(ObserverNodesUp))}),

			% if Master node is down, re-elect a new node
			MasterStatus = check_if_alive(MasterNode),
			if MasterStatus =:= false ->
					io:format("---> No master node, going to elections ~n", []),
					[NewMaster|NewSlaves] = elections(SlaveNodesUp),
					mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NewMaster, slave_nodes=NewSlaves, observer_nodes=ObserverNodesUp}),
					{ready, [NewMaster, NewSlaves, ObserverNodesUp]};
			   true -> 
					{ready, [MasterNode, SlaveNodesUp, ObserverNodesUp]}
			end;
		Error ->
			{error, {not_connected, Error}}
	end.


check_if_alive(NodeName) ->
	NodeResp = net_adm:ping(NodeName),
	if NodeResp =:= pong -> true;
	   true -> false
	end.


%
% Join cluster as observer
%
join_cluster(ClusterName, observer) ->
	join_cluster(ClusterName),
	
	end.

%
% Join cluster as slave
%
join_cluster(ClusterName) ->
	case mnesia:change_config(extra_db_nodes, [ClusterName]) of
		{ok, [_]} -> % connected to cluster, lets read info
			
			% make a copy of the Master MnesiaTable
			mnesia:add_table_copy(ldb_nodes, node(), ram_copies),
			mnesia:add_table_copy(node_info, node(), ram_copies),

			% it is a network, wait for 10 secs
			mnesia:wait_for_tables([ldb_nodes, node_info], 10000),

			case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
				[{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes}] ->

					mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=sets:to_list(sets:from_list(SlaveNodes ++ [node()]))}),
					% start replicating data
					replicate(),

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
			{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes} = Data,
			% TODO: remove node() from list
			
			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=SlaveNodes, observer_nodes=ObserverNodes}),
			{okm disconnected_from_cluster};
		Error ->
			{error, {not_connected, Error}}
	end.

whois_Master() ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		[{ldb_nodes, "ldbNodes", MasterNode, _, _}] -> 
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
			mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NodeName, slave_nodes=[], observer_nodes=[]}),
			{ok, i_am_master};

		%% if node is a slave
		%% remove him from there!
		[{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes}] ->
			NodeIsSlave = (SlaveNodes =/= ([NodeName] -- SlaveNodes)),
			if NodeIsSlave =:= true  ->
					mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NodeName, 
												  slave_nodes= (SlaveNodes -- [NodeName]) ++ [MasterNode],
												  observer_nodes=ObserverNodes}),
					{ok, i_am_master};
			   true ->
					{error, {already_master, {}}}
			end;
		Error ->
			{error, {not_connected, Error}}
	end.


am_I_Slave(NodeName) ->
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		%% node hasn't joined a cluster 
		[] ->
			{error, {not_connected, {}}};

		[{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes}] ->
			NodeIsSlave = (SlaveNodes =/= ([NodeName] -- SlaveNodes)),
			if NodeIsSlave =:= true  -> yes;
			   true -> no
			end;
		Error ->
			{error, {not_connected, Error}}
	end.

make_Observer(NodeName) ->
	IsMaster = am_I_Master(NodeName),
	case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
		_ when IsMaster =:= yes ->
			{error, {is_master, {}}};

		%% node hasn't joined a cluster 
		[] ->
			{error, {not_connected, {}}};

		%% if node is a slave
		%% remove him from there!
		[{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes}] ->
			NodeIsSlave = am_I_Slave(NodeName),
			if NodeIsSlave =:= true  ->
					mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode
												  slave_nodes=(SlaveNodes -- [NodeName]) ,
												  observer_nodes=ObserverNodes ++ [NodeName]
					{ok, i_am_master};
			   true ->
					{error, {already_master, {}}}
			end;
		Error ->
			{error, {not_connected, Error}}
	end.

replicate() ->
	io:format("--> [Replication Started] ~n", []),

	% replication
	{ok, Master} = whois_Master(),

	NodeStatus = check_if_alive(Master),
	case NodeStatus of 
		true ->
			% replicate
			ok;
	    false -> % elect a new Master
			refresh(),
			replicate()
	end,

	io:format("--> [Replication Finished] ~n", []),

	% Updating replication stamp
	mnesia:dirty_write(#node_info{node_name=node(), replication_time=element(2,now())}).
