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

-export([create/1, update/1, delete/1]).

create(Repo) ->
    download_repo(rorrim_conf:get(Repo)).

update(Repo) ->
    update_repo(rorrim_conf:get(Repo)).

delete(Repo) ->
    delete_repo(rorrim_conf:get(Repo)).


%% -------------------------------------------------------------------
%% Helper functions
%% -------------------------------------------------------------------

download_repo(#repo{scm_type=git, name=Name, scm_url=Url, scm_rev=Rev}) ->
    AppDir = app_dir(Name),
    ok = filelib:ensure_dir(AppDir),
    rorrim_util:sh(?FMT("git clone -n ~s ~s",
                        [Url, filename:basename(AppDir)]),
                   [{cd, ?REPO_DIR}]),
    case Rev of
        {branch, Branch} ->
            rorrim_util:sh(?FMT("git checkout -q origin/~s", [Branch]),
                           [{cd, AppDir}]);
        {tag, Tag} ->
            rorrim_util:sh(?FMT("git checkout -q ~s", [Tag]),
                           [{cd, AppDir}])
    end.

update_repo(#repo{scm_type=git, name=Name, scm_rev=Rev}) ->
    AppDir = app_dir(Name),
    ok = filelib:ensure_dir(AppDir),
    rorrim_util:sh("git fetch origin", [{cd, AppDir}]),
    case Rev of
        {branch, Branch} ->
            rorrim_util:sh(?FMT("git checkout -q origin/~s", [Branch]),
                           [{cd, AppDir}]);
        {tag, Tag} ->
            rorrim_util:sh(?FMT("git checkout -q ~s", [Tag]),
                           [{cd, AppDir}])
    end.

delete_repo(#repo{name=Name}) ->
    AppDir = app_dir(Name),
    rorrim_util:sh(?FMT("rm -rf ~s", [AppDir]), []).

app_dir(RepoName) ->
    ?REPO_DIR ++ rorrim_util:get_list(RepoName).
