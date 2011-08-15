-module(sli_admin).

-export([start_link/2,count_url_per_date/2,remove_last_month/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-record(state, {pid}).
-include("sli.hrl").
-define(SEC_IN_MONTH, 2592000). %% 30 days
-define(REQUEST_TIMEOUT, 30000).
%%% --------------------------------------------------------
%%% External API
%%% --------------------------------------------------------
start_link(Host, Port) ->
      gen_server:start_link({local,?MODULE},?MODULE,[Host, Port],[]).

count_url_per_date(Date1, Date2) ->
    gen_server:call(?MODULE, {count_records, Date1, Date2}, ?REQUEST_TIMEOUT).

remove_last_month(Type) ->
    gen_server:cast(?MODULE, {remove_last_month, Type}).

%%% --------------------------------------------------------
%%% Callbacks
%%% --------------------------------------------------------

init([Host, Port]) ->
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    {ok,#state{pid=Pid}}.
    
handle_call({count_records, Date1, Date2}, _From, State) ->
    SecDate1 = calendar:datetime_to_gregorian_seconds(Date1),
    SecDate2 = calendar:datetime_to_gregorian_seconds(Date2),
    Request = "created:[" ++ integer_to_list(SecDate1) ++ " TO " ++ integer_to_list(SecDate2) ++ "]",
    {ok, List} = riakc_pb_socket:search(State#state.pid, ?SL_BUCKET, Request),
    {reply, length(List), State}.

handle_cast({remove_last_month, Type}, State) ->
    Rtype = 
	case Type of
	    created ->
		"created";
	    got ->
		"got"
	end,
    Now = calendar:datetime_to_gregorian_seconds({erlang:date(),
						  erlang:time()}),
    LastMonth = Now - ?SEC_IN_MONTH,
    Request = Rtype ++ ":[0 TO " ++ integer_to_list(LastMonth) ++ "]",
    {ok, List} = riakc_pb_socket:search(State#state.pid, ?SL_BUCKET, Request),
    remove_all(State#state.pid, List),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(Reason, _State) ->
    error_logger:error_msg("Crash riak client: ~p ~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
remove_all(Pid, [[Bucket, Key]|Other]) ->
    riakc_pb_socket:delete(Pid, Bucket, Key),
    remove_all(Pid, Other);
remove_all(_, []) ->
    error_logger:info_msg("All removed").
