%%% ---------------------------------------------------------------
%%% File    : esli_id_link.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : Generator of new web links id. 
%%%
%%% ---------------------------------------------------------------
-module(esli_id_link).

-export([get_id/1, get_solted_id/1]).

-include("esli.hrl").

get_id(Text) ->
    <<Bits:34, _:94>> = erlang:md5(Text),
    additional_chars([lists:nth(Elem + 1, ?CHARS) || Elem <- transform(Bits)]).
    

get_solted_id(Text1) ->
    F = float_to_list(random:uniform()),
    get_id(Text1 ++ F).

%%% Convert 10 to 64
transform(Bits) ->    
    transform([], Bits).

transform(Num, S) when S < 64 ->
    [S|Num];

transform(Ar,Some) ->
    New = Some rem 64,
    transform([New|Ar],Some div 64).

additional_chars(ConvList) when length(ConvList) =:= ?MAX_LENGTH ->
    ConvList;

additional_chars(ConvList) ->
    additional_chars([lists:nth(1, ?CHARS) | ConvList]).
