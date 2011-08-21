%%% ---------------------------------------------------------------
%%% File    : esli_app.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : 
%%% ---------------------------------------------------------------

-module(esli_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    esli_sup:start_link().

stop(_State) ->
    ok.
