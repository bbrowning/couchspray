%%%-------------------------------------------------------------------
%%% File    : mochiweb_rpc.erl
%%% Author  : Benjamin Browning <ben324@gmail.com>
%%% Description : Wrapper to allow rpc calls to mochiweb_request modules
%%%               since rpc:call doesn't support parameterized modules
%%%
%%% Created : 10 Jul 2009 by Benjamin Browning <ben324@gmail.com>
%%%-------------------------------------------------------------------
-module(mochiweb_rpc).
-export([do_rpc/3]).

do_rpc(Module, Function, Args) ->
    %io:format("Doing rpc: mochiweb_request:~s~n", [Function]),
    apply(Module, Function, Args).
