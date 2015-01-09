-module(mas_msg_queue_len).
-behaviour(exometer_probe).

%% API
-export([behaviour/0, probe_init/3, probe_terminate/1, probe_setopts/3,
         probe_update/2, probe_get_value/2, probe_get_datapoints/1,
         probe_reset/1, probe_sample/1, probe_handle_msg/2,
         probe_code_change/3]).

-record(state, {arena_pids = [] :: list(pid),
                msg_queue_lengths = [] :: list(non_neg_integer())}).

-type name()            :: exometer:name().
-type options()         :: exometer:options().
-type type()            :: exometer:type().
-type mod_state()       :: any().
-type data_points()     :: [atom()].
-type probe_reply()     :: ok
                         | {ok, mod_state()}
                         | {ok, any(), mod_state()}
                         | {noreply, mod_state()}
                         | {error, any()}.
-type probe_noreply()   :: ok
                         | {ok, mod_state()}
                         | {error, any()}.


-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.


-spec probe_init(name(), type(), options()) -> probe_noreply().
probe_init(_Name, _Type, Options) ->
    {arena_pids, ArenaPids} = lists:keyfind(arena_pids, 1, Options),
    {ok, #state{arena_pids = [Pid || {_Name, Pid} <- ArenaPids]}}.


-spec probe_terminate(mod_state()) -> probe_noreply().
probe_terminate(_) ->
    ok.


-spec probe_setopts(exometer:entry(), options(), mod_state()) -> probe_reply().
probe_setopts(_Entry, _Options, State) ->
    {ok, State}.


-spec probe_update(any(), mod_state()) -> probe_noreply().
probe_update(_Val, State) ->
    {ok, State}.


-spec probe_get_value(data_points(), mod_state()) -> probe_reply().
probe_get_value(_Datapoints = [msg_lengths], St) ->
    ZippedList = lists:zip(St#state.arena_pids, St#state.msg_queue_lengths),
    {ok, [{msg_lengths, ZippedList}]}.


-spec probe_get_datapoints(mod_state()) -> {ok, data_points()}.
probe_get_datapoints(_State) ->
    {ok, [msg_lengths]}.


-spec probe_reset(mod_state()) -> probe_noreply().
probe_reset(State) ->
    {ok, State}.


-spec probe_sample(mod_state()) -> probe_noreply().
probe_sample(St = #state{arena_pids = ArenaPids}) ->
    MsgQueueLengths = [begin
                           {message_queue_len, L} =
                               erlang:process_info(Pid, message_queue_len),
                           L
                       end || Pid <- ArenaPids],
    {ok, St#state{msg_queue_lengths = MsgQueueLengths}}.


-spec probe_handle_msg(any(), mod_state()) -> probe_noreply().
probe_handle_msg(_Msg, State) ->
    {ok, State}.


-spec probe_code_change(any(), mod_state(), any()) -> {ok, mod_state()}.
probe_code_change(_, State, _) ->
    {ok, State}.
