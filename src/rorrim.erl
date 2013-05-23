%%-------------------------------------------------------------------
%%% @doc Rorrim
%%%
%%% @since 04-04-2013
%%% @end
%%%-------------------------------------------------------------------

-module(rorrim).

-include("rorrim.hrl").

-export([reload/0, get_repos/2]).


reload() ->
    Repos = lists:map(fun (RepoName) -> list_to_atom(RepoName) end,
                      rorrim_util:get_all_repo_dir()),
    RepoConfs = rorrim_util:get_file_conf(),
    rorrim_util:set_conf(RepoConfs),
    NewRepos = [R#repo.name || R <- RepoConfs],

    {CreateRepos, UpdRepos, DelRepos} = get_repos(Repos, NewRepos),

    % Delete repos
    lists:foreach(fun (RepoName) ->
                         rorrim_scm:delete(RepoName)
                 end, DelRepos),

    % Create repos
    lists:foreach(fun (RepoName) ->
                          rorrim_scm:create(RepoName)
                  end, CreateRepos),

    % Update repos
    lists:foreach(fun (RepoName) ->
                          rorrim_scm:update(RepoName)
                  end, UpdRepos).

get_repos(Repos, NewRepos) ->
    get_repos(Repos, NewRepos, {[], [], []}).

get_repos(Repos, [], {CR, UR, _DR}) ->
    {CR, UR, Repos};
get_repos(Repos, [H | NewRepos], {CR, UR, DR}) ->
    case lists:member(H, Repos) of
        true  -> get_repos(lists:delete(H, Repos), NewRepos, {CR, [H | UR], DR});
        false -> get_repos(Repos, NewRepos, {[H | CR], UR, DR})
    end.


