%%%-------------------------------------------------------------------
%%% @doc Rorrim server
%%% * Runs all the commands
%%% * Maintains all the timers
%%%
%%% @since 04-04-2013
%%% @end
%%%-------------------------------------------------------------------

-module(rorrim_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {}).

%% API function definitions

start_link([]) ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server function definitions

init([]) ->
    % Read the config file
    % Add all the repos to monitor list
    % Update state
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    % Check repo (check if present in config, check if already downloaded)
    % Types: new, modified, old config
        % New config: Download, add timer, update state
        % Modified: Remove timer, run update, update timer, update state
        % Old config: Remove timer, delete repo, update state
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
