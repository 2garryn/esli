%%% ---------------------------------------------------------------
%%% File    : sli.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : Interface module for sli-core. 
%%% ---------------------------------------------------------------

-module(sli).

-export([get_short_link/1, get_full_link/1]).


%%% Get short link Id from full link

get_short_link(LongLink) ->
    sli_riakc_handler:request_short_link(self(), LongLink),
    receive
	{short_link, SLId} ->
	    {short_link, SLId};
	_ ->
	    {error, 501}
    after
	10000 ->
	    {error, 501}
    end.

%%% Get full link from short link Id

get_full_link(SLId) ->
    sli_riakc_handler:request_full_link(self(), SLId),
    receive
	{full_link, FullLink} ->
	    {full_link,FullLink};
	{error, 404} ->
	    {error, 404};
	_ ->
	    {error, 501}
    after
	10000 ->
	    {error, 501}
    end.
    
