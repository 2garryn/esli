%%% File    : sli_checker.erl
%%% Author  : garry <garry@garry-desktop>
%%% Description : 
%%% Created : 12 May 2011 by garry <garry@garry-desktop>

-module(sli_checker).

-include("sli.hrl").

-export([check_short/1, check_and_update_full/1]).

 
check_short(Sh) ->
    F = fun(El) ->
		lists:member(El, ?CHARS)
	end,
    L = lists:filter(F, Sh),
    if 
	length(L) =:= ?MAX_LENGTH ->
	    true;
	true ->
	    false
    end.

check_and_update_full(Full) when length(Full) < ?MAX_LONG_LINK_LENGTH ->
    {true, add_protocol(Full, Full)};

check_and_update_full(_) ->
    false.
    
add_protocol("http://" ++ _Other, Full) ->
    Full;

add_protocol("https://" ++ _Other, Full) ->
    Full;

add_protocol("ftp://" ++ _Other, Full) ->
    Full;

add_protocol("gopher://" ++ _Other, Full) ->
    Full;

add_protocol("mailto://" ++ _Other, Full) ->
    Full;

add_protocol("news://" ++ _Other, Full) ->
    Full;

add_protocol("irc://" ++ _Other, Full) ->
    Full;

add_protocol("telnet://" ++ _Other, Full) ->
    Full;

add_protocol("wais://" ++ _Other, Full) ->
    Full;

add_protocol("xmpp://" ++ _Other, Full) ->
    Full;

add_protocol("file://" ++ _Other, Full) ->
    Full;

add_protocol("data://" ++ _Other, Full) ->
    Full;

add_protocol(_, Full) ->
    "http://" ++ Full.
