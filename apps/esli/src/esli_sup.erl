%%% ---------------------------------------------------------------
%%% File    : esli_sup.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : 
%%% ---------------------------------------------------------------

-module(esli_sup).

-export([start_link/0, init/1]).

-include("esli.hrl").

start_link() ->
    supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 60},
	  [{esli_riakc_sup, {esli_riakc_sup, start_link, []},
	    permanent, 5000, supervisor, []},
	  {esli_riakc_handler, {esli_riakc_handler, start_link, [esli_conf:get_config(riak_host),
							       esli_conf:get_config(riak_port),
							       esli_conf:get_config(worker_set)]},
	    permanent, 5000, worker, []},
	  {esli_web, {esli_web, start, []}, 
	   permanent, 5000, worker, dynamic},
	  {esli_admin, {esli_admin, start_link,  [esli_conf:get_config(riak_host),
						esli_conf:get_config(riak_port)]},
	   permanent, 5000, supervisor, []}
	  ]}}.
