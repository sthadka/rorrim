%% -------------------------------------------------------------------
%% @doc Rorrim SCM
%% Module to download and maintain different software configuration
%% management repositories.
%%
%% Based on rebar deps code.
%%
%% @since 04-04-2013
%% @end
%% -------------------------------------------------------------------
-module(rorrim_scm).

-include("rorrim.hrl").

-export([create/1, status/1, update/1, delete/1]).

create(Repo) ->
    download_repo(rorrim_conf:get(Repo)).

update(Repo) ->
    update_repo(rorrim_conf:get(Repo)).

delete(Repo) ->
    update_repo(rorrim_conf:get(Repo)).

status(Repo) ->
    update_repo(rorrim_conf:get(Repo)).


%% -------------------------------------------------------------------
%% Helper functions
%% -------------------------------------------------------------------

download_repo(#repo{scm_type=git, scm_rev=undefined}=RepoDetails) ->
    download_repo(RepoDetails#repo{scm_rev={branch, "HEAD"}});
download_repo(#repo{scm_type=git, scm_rev=""}=RepoDetails) ->
    download_repo(RepoDetails#repo{scm_rev={branch, "HEAD"}});
download_repo(#repo{scm_type=git, name=Name, scm_url=Url, scm_rev=Rev}) ->
    RepoDir = repo_dir(Name),
    ok = filelib:ensure_dir(RepoDir),
    rorrim_util:sh(?FMT("git clone -n ~s ~s",
                        [Url, filename:basename(RepoDir)]),
                   [{cd, ?REPO_DIR}]),
    case Rev of
        {branch, Branch} ->
            rorrim_util:sh(?FMT("git checkout -q origin/~s", [Branch]),
                           [{cd, RepoDir}]);
        {tag, Tag} ->
            rorrim_util:sh(?FMT("git checkout -q ~s", [Tag]),
                           [{cd, RepoDir}])
    end.


update_repo(#repo{}=RepoDetails) ->
    RepoDetails.

repo_dir(RepoName) ->
    ?REPO_DIR ++ rorrim_util:get_list(RepoName).
