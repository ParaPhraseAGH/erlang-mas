%%%-------------------------------------------------------------------
%%% @author jstypka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. lis 2014 17:05
%%%-------------------------------------------------------------------
-module(mas_reporter).

-behaviour(exometer_report).

-include ("mas.hrl").
-include_lib("exometer/include/exometer.hrl").

%% API
-export([exometer_init/1, exometer_report/5, exometer_subscribe/5,
         exometer_unsubscribe/4, exometer_info/2, exometer_call/3,
         exometer_cast/2, exometer_terminate/2, exometer_setopts/4,
         exometer_newentry/2]).

-record(state, {fds :: dict:dict(),
                config :: config()}).
-type state() :: #state{}.

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec exometer_init(exometer_report:options()) -> {ok, state()}.
exometer_init(Options) ->
    {config, Config} = lists:keyfind(config, 1, Options),
    {ok, #state{fds = dict:new(), config = Config}}.


-spec exometer_report(exometer_report:metric(),
                      exometer_report:datapoint(),
                      exometer_report:value(),
                      exometer_report:extra(),
                      exometer_report:mod_state()) -> {ok, state()}.
exometer_report(Metric, _Datapoint, _Value, Extra, St = #state{fds = Dict}) ->
    FD = dict:fetch(Metric, Dict),
    file:write(FD, io_lib:fwrite("~p ~p\n", [Metric, Extra])),
    {ok, St}.


-spec exometer_subscribe(exometer_report:metric(),
                         exometer_report:datapoint(),
                         exometer_report:interval(),
                         exometer_report:extra(),
                         exometer_report:mod_state()) -> {ok, state()}.
exometer_subscribe(Metric, _Datapoint, _Interval,
                   _Extra, St = #state{fds = Dict}) ->
    FD = create_fd([St#state.config#config.log_dir | Metric]),
    {ok, St#state{fds = dict:store(Metric, FD, Dict)}}.


-spec exometer_unsubscribe(exometer_report:metric(),
                           exometer_report:datapoint(),
                           exometer_report:extra(),
                           exometer_report:mod_state()) -> {ok, state()}.
exometer_unsubscribe(Metric, _Datapoint, _Extra, St = #state{fds = Dict}) ->
    close_file(dict:fetch(Metric, Dict)),
    NewDict = dict:erase(Metric, Dict),
    {ok, St#state{fds = NewDict}}.


-spec exometer_info(any(), exometer_report:mod_state()) ->
                           exometer_report:callback_result().
exometer_info(_, State) ->
    {ok, State}.

-spec exometer_call(any(), pid(), exometer_report:mod_state()) ->
                           {reply, any(), exometer_report:mod_state()} |
                           {noreply, exometer_report:mod_state()} |
                           any().
exometer_call(_, _, State) ->
    {noreply, State}.

-spec exometer_cast(any(), exometer_report:mod_state()) ->
                           {noreply, exometer_report:mod_state()} | any().
exometer_cast(_, State) ->
    {noreply, State}.


-spec exometer_terminate(any(), exometer_report:mod_state()) -> any().
exometer_terminate(_, #state{fds = Dict}) ->
    [close_file(FD) || {_Metric, FD} <- dict:to_list(Dict)].

-spec exometer_setopts(exometer_report:metric(),
                       exometer_report:options(),
                       exometer:status(),
                       exometer_report:mod_state()) ->
                              exometer_report:callback_result().
exometer_setopts(_, _, _, State) ->
    {ok, State}.

-spec exometer_newentry(#exometer_entry{}, exometer_report:mod_state()) ->
                               exometer_report:callback_result().
exometer_newentry(_, State) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_fd([string() | atom()]) -> standard_io | file:fd().
create_fd([standard_io | _]) ->
    standard_io;

create_fd(["standard_io" | _]) ->
    standard_io;

create_fd([Path | []]) ->
    file:open(Path ++ ".txt", [append, delayed_write, raw]);

create_fd([Folder | Path]) ->
    case file:make_dir(Folder) of
        ok -> ok;
        {error, eexist} -> already_exists;
        {error, Reason} -> erlang:error(Reason)
    end,
    [Next | Rest] = Path,
    create_fd([filename:join([Folder, Next]) | Rest]).


-spec close_file(standard_io | file:fd()) -> ok.
close_file(standard_io) ->
    ok;

close_file(FD) ->
    file:close(FD).