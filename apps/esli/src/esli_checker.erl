%%% ---------------------------------------------------------------
%%% File    : esli_checker.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : 
%%% ---------------------------------------------------------------


-module(esli_checker).

-include("esli.hrl").

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
    case check_1(Full) of
	{true, NewStr} ->
	    {true, add_protocol(NewStr, NewStr)};
	false ->
	    false
    end;

check_and_update_full(_) ->
    false.


%% check that full url is not space
check_1(Full) ->
    case string:strip(Full, both, 32) of
	[] ->
	    false;
	NewStr ->
	    check_2(NewStr)
    end.


%% url should not contain spaces
check_2(Full) ->
    case length(string:tokens(Full, " ")) of
	1 ->
	    {true, Full};
	_ ->
	    false
    end.

%% add protocol if it is not in URL
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
