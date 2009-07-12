%%%-------------------------------------------------------------------
%%% File    : couchspray.erl
%%% Author  : Benjamin Browning <ben324@gmail.com>
%%% Description : CouchDB Partitioning Proxy
%%%
%%% Created :  8 Jul 2009 by Benjamin Browning <ben324@gmail.com>
%%% License : Apache 2.0
%%%-------------------------------------------------------------------
-module(couchspray).
-author("ben324@gmail.com").
-vsn("0.1").

% CouchSpray is designed to run inside the same beam process as CouchDB.
% Make sure couchspray is in the Erlang load path and ensure the following
% line points to CouchDB's hrl file.
-include("src/couchdb/couch_db.hrl").

-export([handle_request/1]).
-export([lookup_node/1]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1,
    start_chunked_response/3, absolute_uri/2]).

% Database request handlers
handle_request(#httpd{path_parts=[_DbName|RestParts],method=Method,
        db_url_handlers=DbUrlHandlers}=Req)->
    case {Method, RestParts} of
    {'PUT', []} ->
        couch_httpd_db:handle_request(Req);
    {'DELETE', []} ->
        couch_httpd_db:handle_request(Req);
    {_, []} ->
        do_db_req(Req, fun db_req/2);
    {_, [SecondPart|_]} ->
        Handler = couch_util:dict_find(SecondPart, DbUrlHandlers, fun db_req/2),
        do_db_req(Req, Handler)
    end.

do_db_req(#httpd{user_ctx=UserCtx,path_parts=[DbName|_]}=Req, Fun) ->
    case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
    {ok, Db} ->
        try
            Fun(Req, Db)
        after
            catch couch_db:close(Db)
        end;
    Error ->
        throw(Error)
    end.

db_req(#httpd{method='GET',path_parts=[_DbName]}=Req, Db) ->
    % Dunno what to return for sharded databases
    {ok, DbInfo} = couch_db:get_db_info(Db),
    send_json(Req, {DbInfo});

db_req(#httpd{method='POST',path_parts=[DbName]}=Req, _Db) ->
    DocId = couch_util:new_uuid(),
    proxy_req(Req#httpd{method='PUT', path_parts=[DbName, DocId]}, DocId);

db_req(#httpd{path_parts=[_DbName]}=Req, _Db) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST");

db_req(#httpd{path_parts=[_, DocId]}=Req, Db) ->
    db_doc_req(Req, Db, DocId).

db_doc_req(#httpd{method='DELETE'}=Req, _Db, DocId) ->
    proxy_req(Req, DocId);

db_doc_req(#httpd{method='GET'}=Req, _Db, DocId) ->
    proxy_req(Req, DocId);

db_doc_req(#httpd{method='POST'}=Req, _Db, DocId) ->
    proxy_req(Req, DocId);

db_doc_req(#httpd{method='PUT'}=Req, _Db, DocId) ->
    proxy_req(Req, DocId).

proxy_req(Req, DocId) ->
    Node = lookup_node(DocId),
    MochiReq = Req#httpd.mochi_req,
    DistribMochiReq = distrib_mochiweb_request:new(node(), MochiReq),
    rpc:call(Node, couch_httpd_db, handle_request, [Req#httpd{mochi_req=DistribMochiReq}]).

lookup_node(_DocId) ->
    % Obviously this is a hack just for testing - need to integrate
    % chash or some other library for consistent hashing of docs to nodes
    couch1@macbook.
    
