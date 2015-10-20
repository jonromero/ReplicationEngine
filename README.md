[![Build Status](https://travis-ci.org/jonromero/ReplicationEngine.svg)](https://travis-ci.org/jonromero/ReplicationEngine)

DanceCommander / ReplicationEngine 
========================

A very simple (yet effective) replication (?) system 

# Some info --
DanceCommander provides replication features to servers along with automatic leader election and server presence.
It aims to be a very simple set-and-forget service.

Motto: Less is more

# Architectural goals
+ Many DanceCommanders can be used (no single point of failure)
+ Replication is transparent
+ Elections are transparent and automatic
+ Keep it simple

# How it works
A DanceCommander starts and many nodes are connected (joining the cluster).
One of the nodes is Master and the rest are the Slaves.
The DanceCommanders are the Observers.

Every N seconds (based on the DanceCommander's config), all the LIFF (latest) files
are copied from Master to Slaves.

Promoting a Slave to Master translates to restarting the Slave node.
Master nodes that have failed and returned, cannot join the cluster automatically.
They must join the cluster as Slaves, sync data and then get promoted.


Jon - https://twitter.com/jonromero


