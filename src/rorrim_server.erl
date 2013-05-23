%%%-------------------------------------------------------------------
%%% @doc Rorrim server
%%% * Runs all the commands
%%% * Maintains all the timers
%%%
%%% @since 04-04-2013
%%% @end
%%%-------------------------------------------------------------------

-module(rorrim_server).

-include("rorrim.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(rp, {name, interval, timer}).
-record(state, {repos=[]}).

%% API function definitions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server function definitions

init([]) ->
    rorrim:reload(),
    ReposConf = rorrim_util:get_file_conf(),
    Repos = lists:foldl(fun (RepoConf, RPs) ->
                                Interval = RepoConf#repo.interval * 1000, % in seconds
                                Ref = erlang:start_timer(Interval, self(), RepoConf#repo.name),
                                RP = #rp{name = RepoConf#repo.name,
                                         interval = Interval,
                                         timer = Ref},
                                [RP | RPs]
                        end, [], ReposConf),
    {ok, #state{repos=Repos}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    % TODO
    % Check repo (check if present in config, check if already downloaded)
    % Types: new, modified, old config
        % New config: Download, add timer, update state
        % Modified: Remove timer, run update, update timer, update state
        % Old config: Remove timer, delete repo, update state
    {noreply, State}.

handle_info({timeout, Ref, RepoName}, #state{repos=Repos}=State) ->
    {value, Repo} = {value, #rp{timer=Ref}} = lists:keysearch(RepoName, 2, Repos),
    NewRef = erlang:start_timer(Repo#rp.interval, self(), Repo#rp.name),
    rorrim_scm:update(RepoName),
    NewRepos = [Repo#rp{timer=NewRef} | lists:keydelete(RepoName, 2, Repos)],
    {noreply, State#state{repos=NewRepos}};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
