## Welcome to Couchspray

Eventually Couchspray aims to be a library for sharding CouchDB nodes. For now,
it's a heavy work-in-progress.

## Basic Usage

NOTE: Couchspray is only compatible with CouchDB trunk.

The easiest way to try Couchspray is to put the couchspray directory
inside the CouchDB trunk directory and modify the httpd default handler
to use couchspray for all http requests.

    erlc couchspray/*.erl
    make dev
    modify etc/couchdb/local_dev.ini and add the lines
        default_handler = {couchspray, handle_request}
        [httpd_global_handlers]
        _all_dbs = {couchspray, handle_all_dbs_req}
    ERL_FLAGS="-sname couchspray -cookie test" utils/run -i

For Couchspray to be useful, you need to start a second CouchDB node.
Check out CouchDB trunk and Couchspray to another directory and
start as before, but this time don't modify the http request handler.

    erlc couchspray/*.erl
    make dev
    ERL_FLAGS="-sname couch1 -cookie test" utils/run -i

Right now Couchspray is hardcoded to send all requests to 'couch1@macbook'.
It also expects your Couches to be started in distributed mode. I need to
update these directions to explain how to do that.

My first goal is getting simple single document CRUD operations sharded
and then I'll move on to views and such. The README will be updated as
things get more ready for general use.

## Credits
J. Chris Anderson - Instructions for running Couchspray are largely
copied from his Hovercraft README.md

Apache 2.0 License
Copyright 2009 Ben Browning <ben324@gmail.com>