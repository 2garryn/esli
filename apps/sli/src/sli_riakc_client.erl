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
    Obj = riakc_obj:new(?SL_BUCKET, list_to_binary(EndedId), list_to_binary(LongLink)),
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
	{ok, Data} ->
	    BinData =  riakc_obj:get_value(Data),
	    ReqPid ! {full_link, binary_to_list(BinData)};
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
