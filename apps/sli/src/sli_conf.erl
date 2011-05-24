%%% ---------------------------------------------------------------
%%% File    : sli_conf.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : Helper for getting settings from app.config
%%% ---------------------------------------------------------------

-module(sli_conf).

-export([get_config/1]).

get_config(Var) ->
    {ok, Value} = application:get_env(sli, Var),
    Value.


