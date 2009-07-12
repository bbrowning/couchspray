%%%-------------------------------------------------------------------
%%% File    : distrib_mochiweb_request.erl
%%% Author  : Benjamin Browning <ben324@gmail.com>
%%% Description : mochiweb_request wrapper to let it work distributed
%%%
%%% Created : 10 Jul 2009 by Benjamin Browning <ben324@gmail.com>
%%% original author: Bob Ippolito x<bob@mochimedia.com>
%%% original copyright 2007 Mochi Media, Inc.
%%%-------------------------------------------------------------------
-module(distrib_mochiweb_request, [Node, MochiReq]).
-author("ben324@gmail.com").

-export([get_header_value/1, get_primary_header_value/1, get/1, dump/0]).
-export([send/1, recv/1, recv/2, recv_body/0, recv_body/1, stream_body/3]).
-export([start_response/1, start_response_length/1, start_raw_response/1]).
-export([respond/1, ok/1]).
-export([not_found/0, not_found/1]).
-export([parse_post/0, parse_qs/0]).
-export([should_close/0, cleanup/0]).
-export([parse_cookie/0, get_cookie_value/1]).
-export([serve_file/2, serve_file/3]).


%% TODO: Look into smerl or other metaprogramming to replace all this junk
%% TODO: Figure out which things really need to use rpc:call and which
%%       can just be executed locally (parse_qs is good example)

get_header_value(K) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, get_header_value, [K]]).

get_primary_header_value(K) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, get_primary_header_value, [K]]).

get(Key) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, get, [Key]]).

dump() ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, []]).

send(Data) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, send, [Data]]).

recv(Length) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, recv, [Length]]).

recv(Length, Timeout) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, recv, [Length, Timeout]]).

recv_body() ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, recv_body, []]).

recv_body(MaxBody) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, recv_body, [MaxBody]]).

stream_body(MaxChunkSize, ChunkFun, FunState) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, stream_body, [MaxChunkSize, ChunkFun, FunState]]).

start_response(Params) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, start_response, [Params]]).

start_raw_response(Params) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, start_raw_response, [Params]]).

start_response_length(Params) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, start_response_length, [Params]]).

respond(Params) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, respond, [Params]]).

not_found() ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, not_found, []]).

not_found(ExtraHeaders) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, not_found, [ExtraHeaders]]).

ok(Params) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, ok, [Params]]).

should_close() ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, should_close, []]).

cleanup() ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, cleanup, []]).

parse_qs() ->
    MochiReq:parse_qs().
    %rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, parse_qs, []]).

get_cookie_value(Key) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, get_cookie_value, [Key]]).

parse_cookie() ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, parse_cookie, []]).

parse_post() ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, parse_post, []]).

serve_file(Path, DocRoot) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, serve_file, [Path, DocRoot]]).

serve_file(Path, DocRoot, ExtraHeaders) ->
    rpc:call(Node, mochiweb_rpc, do_rpc, [MochiReq, serve_file, [Path, DocRoot, ExtraHeaders]]).

