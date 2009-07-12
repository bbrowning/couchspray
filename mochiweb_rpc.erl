%%%-------------------------------------------------------------------
%%% File    : mochiweb_rpc.erl
%%% Author  : Benjamin Browning <ben@macbook.local>
%%% Description : 
%%%
%%% Created : 10 Jul 2009 by Benjamin Browning <ben@macbook.local>
%%%-------------------------------------------------------------------
-module(mochiweb_rpc).
-export([do_rpc/3]).

do_rpc(Module, Function, Args) ->
    apply(Module, Function, Args).
