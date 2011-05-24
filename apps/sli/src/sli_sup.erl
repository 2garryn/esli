%%% ---------------------------------------------------------------
%%% File    : sli_sup.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : 
%%% ---------------------------------------------------------------

-module(sli_sup).

-export([start_link/0, init/1]).

-include("sli.hrl").

start_link() ->
    supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 60},
	  [{sli_id_link, {sli_id_link, start_link, []},
	    permanent, 5000, worker, []},
	  {sli_riakc_sup, {sli_riakc_sup, start_link, []},
	    permanent, 5000, supervisor, []},
	  {sli_riakc_handler, {sli_riakc_handler, start_link, [sli_conf:get_config(riak_host),
							       sli_conf:get_config(riak_port),
							       sli_conf:get_config(worker_set)]},
	    permanent, 5000, worker, []},
	  {sli_web, {sli_web, start, []}, 
	   permanent, 5000, worker, dynamic}
	  ]}}.
