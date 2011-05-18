-module(sli_id_link).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_link/0, get_next_id/0]).

-include("sli.hrl").


-record(state, {last_id, fd}).

%% API

start_link() ->
    gen_server:start_link({local, ?ID_LINK}, ?MODULE, [], []).

get_next_id() ->
    gen_server:call(?ID_LINK, get_next_id).

%% Callbacks

init([]) ->
    process_flag(trap_exit, true),
    case file:open(sli_conf:get_config(id_file_place),[write, read, raw]) of
	{ok, Fd} ->
	    State = make_state(Fd),
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_call(get_next_id, _From, State) ->
    NextId = next_id_processing(State#state.last_id),
    {reply, NextId, State#state{last_id=NextId}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    update_file(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

make_state(Fd) ->
    {ok,Line} = file:read_line(Fd),
    NextId = string:sub_string(Line, 1, 6),
%    NextId =  next_id_processing(ClearData),
    #state{last_id=NextId,
	   fd=Fd}.
%%%
%%% 48 - code of "0"
%%% 97 - code of "a"
%%%



next_id_processing([48,48,48,48,48,48]) ->
    [97,97,97,97,97,97];

next_id_processing([A,48,48,48,48,48]) ->
    Nc = next_char(A),
    [Nc, 97,97,97,97,97];

next_id_processing([A,B,48,48,48,48]) ->
    Nc = next_char(B),
    [A,Nc,97,97,97,97];

next_id_processing([A,B,C,48,48,48]) ->
    Nc = next_char(C),
    [A,B,Nc,97,97,97];

next_id_processing([A,B,C,D,48,48]) ->
    Nc = next_char(D),
    [A,B,C,Nc,97,97];

next_id_processing([A,B,C,D,E,48]) ->
    Nc = next_char(E),
    [A,B,C,D,Nc,97];

next_id_processing([A,B,C,D,E,F]) ->
    Nc = next_char(F),
    [A,B,C,D,E,Nc].

next_char(Ch) ->
    next_char2(Ch,?CHARS).

next_char2(Ch, [Ch|List]) ->
    [N|_] = List,
    N;
next_char2(Ch, [_|List]) ->
    next_char2(Ch, List).


update_file(State) ->
    file:pwrite(State#state.fd, 0, State#state.last_id),
    file:close(State#state.fd).

