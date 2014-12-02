-module(mas_vm_probe).
-behaviour(exometer_probe).

%% API
-export([behaviour/0, probe_init/3, probe_terminate/1, probe_setopts/3,
         probe_update/2, probe_get_value/2, probe_get_datapoints/1,
         probe_reset/1, probe_sample/1, probe_handle_msg/2,
         probe_code_change/3]).

-record(state, {old_walltime :: [tuple()],
                scheduler_wt :: [tuple()]}).

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
probe_init(_Name, _Type, _Options) ->
    erlang:system_flag(scheduler_wall_time, true),
    Walltime = lists:sort(erlang:statistics(scheduler_wall_time)),
    {ok, #state{old_walltime = Walltime}}.


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
probe_get_value(_Datapoints = [scheduler_wt], State) ->
    {ok, [{scheduler_wt, State#state.scheduler_wt}]}.


-spec probe_get_datapoints(mod_state()) -> {ok, data_points()}.
probe_get_datapoints(_State) ->
    {ok, [scheduler_wt]}.


-spec probe_reset(mod_state()) -> probe_noreply().
probe_reset(State) ->
    {ok, State}.


-spec probe_sample(mod_state()) -> probe_noreply().
probe_sample(St = #state{old_walltime = OldWalltime}) ->
    NewWalltime = lists:sort(erlang:statistics(scheduler_wall_time)),
    NewSchedulerWt = lists:map(fun({{Sch, A0, T0}, {Sch, A1, T1}}) ->
                                       {Sch, (A1 - A0)/(T1 - T0)}
                               end,
                               lists:zip(OldWalltime, NewWalltime)),
    {ok, St#state{old_walltime = NewWalltime,
                  scheduler_wt = NewSchedulerWt}}.


-spec probe_handle_msg(any(), mod_state()) -> probe_noreply().
probe_handle_msg(_Msg, State) ->
    {ok, State}.


-spec probe_code_change(any(), mod_state(), any()) -> {ok, mod_state()}.
probe_code_change(_, State, _) ->
    {ok, State}.
