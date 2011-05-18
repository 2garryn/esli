%%% File    : sli_conf.erl
%%% Author  : garry <garry@garry-desktop>
%%% Description : 
%%% Created : 10 May 2011 by garry <garry@garry-desktop>

-module(sli_conf).

-export([get_config/1]).

get_config(Var) ->
%    "/var/mochiweb/docs";
    {ok, Value} = application:get_env(sli, Var),
    Value.


