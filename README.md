ReplicationEngine
========================

A very simple (yet effective) replication system 

# Some info
This is the experimental branch for λDB (BugSense's internal database).
Expect to see lots of stuff changing all the time, formats being broken etc.

Motto: Less is more

# Architectural goals
+ Decentralized (no master)
+ Distributed
+ Homogeneous (all nodes can do anything)
+ Fault tolerant

# How it works
Read more at BugSense's post: http://blog.bugsense.com/post/30522575208/big-data-and-replication-challenge-accepted

Every node is the same. When the Master node goes down, the last-to-data-written replication node,
is promoted to Master. When the Master node comes back, it is just a new replication node.

# Pros
+ Simple/fast implementation (a.k.a less bugs)
+ Minimal downtime of the service
+ Best ration consistency/speed

# Cons
+ NO guarandeed 100% consistency (there are more complicated algorithms and way better designed. Read: PAXOS).


Jon - https://twitter.com/jonromero


