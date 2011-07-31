%%% ---------------------------------------------------------------
%%% File    : sli_riakc_client.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : It is wrapper for riakc_pb client
%%% ---------------------------------------------------------------

-module(sli_riakc_client).

-behaviour(gen_server).

%% API
-export([start_link/3, get_short_link/3, get_full_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {pid, name, req_id}).

-include("sli.hrl").

%% -------------------------------------------
%% Interface function
%% -------------------------------------------
start_link(Name, Host, Port) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Host, Port], []).

get_short_link(FreeW, ReqPid, LongLink) ->
    gen_server:cast(FreeW, {get_short_link, ReqPid, LongLink}).     

get_full_link(Free, ReqPid, ShortLink) ->
    gen_server:cast(Free, {get_full_link, ReqPid, ShortLink}).

%% ------------------------------------------
%% Callback
%% ------------------------------------------

init([Name, Host, Port]) ->
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    ?RC_HANDLER ! {free, Name},
    {ok, #state{pid=Pid, name=Name}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({get_short_link, ReqPid, LongLink}, State) ->
    EndedId =  get_my_id(LongLink, State#state.pid),
    Obj = riakc_obj:new(?SL_BUCKET, 
                        list_to_binary(EndedId), 
                        json_view(list_to_binary(LongLink), list_to_binary(EndedId)), 
                        "application/json"),
    case riakc_pb_socket:put(State#state.pid, Obj) of 
	ok ->
	    ReqPid ! {short_link, EndedId};
	SomeThing ->
	    error_logger:error_msg("Put to riak error: ~p ~n", [SomeThing]),
	    ReqPid ! error
    end,
    ?RC_HANDLER ! {free, State#state.name},
    {noreply, State};


handle_cast({get_full_link, ReqPid, ShortLink}, State) ->
    case riakc_pb_socket:get(State#state.pid,?SL_BUCKET, list_to_binary(ShortLink)) of
	{ok, RiakData} ->
	    Full = get_full_from_obj(RiakData),
	    ReqPid ! {full_link, binary_to_list(Full)},
	    update_got_date(RiakData, State#state.pid);
	_ ->
	    ReqPid ! {error, 404}
    end,
    ?RC_HANDLER ! {free, State#state.name},
    {noreply, State}.  

handle_info(_Info, State) ->
    {noreply, State}.


terminate(Reason, _State) ->
    error_logger:error_msg("Crash riak client: ~p ~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_my_id(Text, Pid) -> 
    get_my_pid(Text, sli_id_link:get_id(Text), Pid).
    
get_my_pid(Text, LinkId, Pid) ->
    case riakc_pb_socket:get(Pid, ?SL_BUCKET,  list_to_binary(LinkId)) of
	{error, notfound} ->
	    LinkId;
	_ ->
	    NewLI = sli_id_link:get_solted_id(Text),
	    get_my_pid(Text, NewLI, Pid)
    end.    




%%% ---------------------------------------------
%%% JSON view of long link
%%% 
%%% {link:  "http://long_link",
%%%  short: "qWeD12",
%%%  created: 123123123, - unix epoch
%%%  updated: 123123123}

json_view(Long, Short) ->
    UnixEpoch = unix_epoch_now(),
    Json = struct(Long, UnixEpoch, UnixEpoch, Short), 
    iolist_to_binary(mochijson2:encode(Json)).


struct(Long,Created, Updated, Short) ->
    {struct,
       [
	 {link, Long},
         {short,Short},
         {created, Created},
         {got, Updated}
       ]
    }. 

unix_epoch_now() ->
    Date = erlang:date(),
    Time = erlang:time(),
    calendar:datetime_to_gregorian_seconds({Date,Time}).    
    
get_full_from_obj(RiakData) ->
    case riakc_obj:get_content_type(RiakData) of
	"application/json" ->
	    io:format("APP JSON"),
	    {struct, Fields} = mochijson2:decode(riakc_obj:get_value(RiakData)),
	    io:format("FIELDS = ~p ~n",[Fields]),
	    proplists:get_value(<<"link">>, Fields, "not_found");
	_ ->
	    io:format("STRING ~n"),
	    riakc_obj:get_value(RiakData)
    end.
	    
	

update_got_date(RiakData, Pid) ->
    PutToRiak = 
    case riakc_obj:get_content_type(RiakData) of
	"application/json" ->
	    {struct, Fields} = mochijson2:decode(riakc_obj:get_value(RiakData)),
	    NewList = lists:keyreplace(<<"got">>, 1, Fields, {<<"got">>, unix_epoch_now()}),
	    NewObj = iolist_to_binary(mochijson2:encode({struct, NewList})),
	    riakc_obj:update_value(RiakData, NewObj);
	_ ->
	    NewJson = json_view(riakc_obj:get_value(RiakData), riakc_obj:key(RiakData)),
	    riakc_obj:update_value(RiakData, NewJson, "application/json")
    end,
    riakc_pb_socket:put(Pid, PutToRiak).
	    
	    


    
    
    

