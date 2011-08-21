%%% ---------------------------------------------------------------
%%% File    : esli_conf.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : Helper for getting settings from app.config
%%% ---------------------------------------------------------------

-module(esli_conf).

-export([get_config/1]).

get_config(Var) ->
    {ok, Value} = application:get_env(esli, Var),
    Value.


