-module(sli_riakc_handler).

-behaviour(gen_server).

%%% Defines

-include("sli.hrl").
%% API
-export([start_link/3, request_short_link/2, request_full_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(workers, {free=[],
		  busy=[],
		  read_queue=[],
		  write_queue=[]
		 }).
%%% --------------------------------------------------------
%%% External API
%%% --------------------------------------------------------

start_link(Host, Port, Number) ->
    gen_server:start_link({local, ?RC_HANDLER}, ?MODULE, [Host, Port, Number], []).

request_short_link(ReqPid, LongLink) ->
    gen_server:cast(?RC_HANDLER, {req_short, ReqPid, LongLink}).

request_full_link(ReqPid, ShLink) ->
    gen_server:cast(?RC_HANDLER, {req_full, ReqPid, ShLink}).
    

%%% --------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------



init([Host, Port, Number]) ->
    start_processes(Host, Port, Number),
    State = #workers{free=[], busy=[]},
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({req_short, ReqPid, LongLink}, State) ->
    case get_free(State) of
	{ok, Free, NewState} ->
	    send2worker(Free, ReqPid, LongLink);
	{error, NewState} ->
	    error_logger:error_msg("No free workers ~n"),
	    ReqPid ! error
    end,
    {noreply, NewState};

handle_cast({req_full, ReqPid, ShortLink}, State) ->
    case get_free(State) of
	{ok, Free, NewState} ->
	    get_from_worker(Free, ReqPid, ShortLink);
	{error, NewState} ->
	    error_logger:error_msg("No free workers ~n"),
	    ReqPid ! error
    end,
    {noreply, NewState}.

handle_info({free, Id}, State) ->
    NewState = make_free(Id, State),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% --------------------------------------------
%%% Internal functions
%%% --------------------------------------------

start_processes(_Host, _Port, 0) ->
    ok;

start_processes(Host, Port, Number) ->
    NewName = list_to_atom(?WC_NAME ++ integer_to_list(Number)),
    case supervisor:start_child(?SV_NAME, get_child_spec(Host, Port, NewName)) of
	{ok, Child} ->
	    error_logger:info_msg("Start child proc: ~n name : ~p ~n child : ~p ~n",[NewName, Child]),
	    start_processes(Host, Port, Number - 1);
	{ok, Child, Info} ->
	    error_logger:info_msg("Start reader child proc: ~n child : ~p ~n info : ~p ~n ~n",[NewName, Child, Info]),
	    start_processes(Host, Port, Number - 1);
	{error, Error} ->
	    error_logger:error_msg("Error reader child proc start with: ~n name : ~p ~n error ~p  ~n",[NewName, Error]),
	    erlang:error({NewName, Error})
    end.


get_free(State) ->
    case State#workers.free of 
	[] ->
	    {error, State};
	[One|Free] ->
	    #workers{busy=Busy} = State,
	    NewState = State#workers{free=Free,
				     busy=[One|Busy]},
	    {ok, One, NewState}
    end.


send2worker(FreeW, ReqPid, LongLink) ->
    NextId = sli_id_link:get_next_id(),
    sli_riakc_client:get_short_link(FreeW, ReqPid, LongLink, NextId).

get_from_worker(FreeR, ReqPid, ShortLink) ->
    sli_riakc_client:get_full_link(FreeR, ReqPid, ShortLink).

make_free(Id, State) ->
    NewBusy = lists:delete(Id, State#workers.busy),
    NewFree = [Id|State#workers.free],
    State#workers{busy=NewBusy,
		  free=NewFree}.
    
get_child_spec(Host, Port, NewName) ->
    {NewName, {sli_riakc_client, start_link, [NewName, Host, Port]}, transient, brutal_kill, worker, [sli_riakc_client]}.

