%%%-------------------------------------------------------------------
%%% File    : sli_riakc_sup.erl
%%% Author  : garry <garry@garry-desktop>
%%% Description : 
%%%
%%% Created : 31 Mar 2011 by garry <garry@garry-desktop>
%%%-------------------------------------------------------------------
-module(sli_riakc_sup).

-behaviour(supervisor).

-include("sli.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?SV_NAME}, ?MODULE, []).

init([]) ->
    {ok,{{one_for_one,10,10}, []}}.

