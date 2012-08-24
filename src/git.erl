-module(git).

-export([clone/2,
         fetch/1,
         checkout/2,

         status_is_dirty/1,
         status_changed_files/1,
         log_commits/1,
         describe/1, semver/1,
         head/1,

         refs/1,

         remotes/1,  remotes_commits/1,
         tags/1,     tags_commits/1,
         branches/1, branches_commits/1,
         branch/1,

         version_tags/1, version_tags_commits/1,
         get_reachable_versions/1,

         diff_names/3,

         add_files/2,
         commit/2, amend_changes/1,
         tag/2,
         reset_hard/2
        ]).

-export([clone_cmd/2,
         fetch_cmd/1,
         checkout_cmd/2]).

-import(git_utils, [fformat/2, strip/1, join/2]).

-include_lib("erlsemver/include/semver.hrl").

%% =============================================================================
%%
%% API
%%
%% =============================================================================

%% @throws {unable_to_clone, Reason :: list()}>
-spec clone(list(), list()) -> {'ok', string()}.
clone(RepoURL, RepoPath) ->
    sh(clone_cmd(RepoURL, RepoPath), []).

clone_cmd(RepoURL, RepoPath) ->
    fformat("git clone \"~s\" \"~s\"", [RepoURL, RepoPath]).

%% @doc Fetches recent changes from repo.
%% @throws {unable_to_checkout, Reason}
-spec fetch(list()) -> {'ok', string()}.
fetch(RepoDir) ->
    sh(fetch_cmd(RepoDir), [{cd, RepoDir}]).

fetch_cmd(_RepoDir) ->
    "git fetch".

%% @doc Tries to checkout to given commit.
%% @throws {unable_to_checkout, Reason}
-spec checkout(list(), string()) -> {'ok', string()}.
checkout(RepoDir, CommitID) ->
    sh(checkout_cmd(RepoDir, CommitID), [{cd, RepoDir}]).

checkout_cmd(_RepoDir, CommitID) ->
    fformat("git checkout -f ~s", [CommitID]).

-spec branch(list()) -> string().
branch(Repo) ->
    H = head(Repo),
    hd([ N || {N, T, C} <- refs(Repo), T == head, C == H ]).

-spec branches(list()) -> list(string()).
branches(Repo) ->
    [ N || {N, T, _C} <- refs(Repo), T == head ].
-spec branches_commits(list()) -> list({string(), string()}).
branches_commits(Repo) ->
    [ {N, C} || {N, T, C} <- refs(Repo), T == head ].

-spec refs(list()) -> list({string(), atom(), string()}).
refs(Repo) ->
    Output = oksh(refs_cmd(Repo), []),
    lists:map(fun(L) ->
                      [Commit, Ref] = string:tokens(L, "\t "),
                      {Type, Name} = case string:tokens(Ref, "/") of
                                         ["refs", T | N] ->
                                             {T, string:join(N, "/")};
                                         ["HEAD"] ->
                                             {"HEAD", "HEAD"}
                                     end,
                      {Name, list_to_atom(string:strip(Type, right, $s)), Commit}
              end, string:tokens(Output, [13,10])).

refs_cmd(Repo) ->
    fformat("git ls-remote ~s", [Repo]).

status_is_dirty(Repo) ->
    case sh("git status --porcelain | egrep -v \"^\\?\\?\"", [{cd, Repo}]) of
        "" ->
            false;
        _ ->
            true
    end.

status_changed_files(Repo) ->
    status_changed_files(Repo, ".").

add_files(Repo, Files) ->
    add_files(Repo, Files, ".").

-spec remotes(list()) -> list(string()).
remotes(Repo) ->
    [ N || {N, T, _C} <- refs(Repo), T == remote ].
-spec remotes_commits(list()) -> list({string(), string()}).
remotes_commits(Repo) ->
    [ {N, C} || {N, T, C} <- refs(Repo), T == remote ].

-spec tags(list()) -> list(string()).
tags(Repo) ->
    [ N || {N, T, _C} <- refs(Repo), T == tag ].
-spec tags_commits(list()) -> list({string(), string()}).
tags_commits(Repo) ->
    [ {N, C} || {N, T, C} <- refs(Repo), T == tag ].

version_tags(Repo) ->
    [ semver:from_str(V) || [$v | V ] <- tags(Repo) ].

version_tags_commits(Repo) ->
    get_commits(Repo, version_tags(Repo)).

get_reachable_versions(Repo) ->
    Tags = version_tags(Repo),
    get_reachable_tags(Repo, Tags).


commit(Repo, Msg) ->
    sh("git commit -m \"~s\"", [Msg], [{cd, Repo}]).

amend_changes(Repo) ->
    sh("git show HEAD --pretty=%s%n%n%b --summary | git commit -F - --amend", [{cd, Repo}]).

tag(Repo, Ver) ->
    sh("git tag -f v~s", [semver:to_str(Ver)], [{cd, Repo}]),
    Ver.

log_commits(Repo) ->
    string:tokens(oksh("git log --format=\"%H\"", [{cd, Repo}]), "\n").

describe(Repo) ->
    describe_tags(Repo).

describe_tags(Repo) ->
    strip(oksh("git describe --tags", [{cd, Repo}])).

semver(Repo) ->
    semver:from_git_describe(describe_tags(Repo)).

diff_names(Repo, A, B) ->
    diff_names(Repo, A, B, ".").

head(Repo) ->
    strip(oksh("git rev-parse HEAD", [{cd, Repo}])).

reset_hard(Repo, #semver{} = Ver) ->
    reset_hard(Repo, semver:to_tag(Ver));
reset_hard(Repo, Commit) ->
    strip(oksh("git reset --hard ~s", [Commit], [{cd, Repo}])).

%% =============================================================================
%%
%% Internal
%%
%% =============================================================================

change_type("A\t") ->
    indexed_added;
change_type("A ") ->
    indexed_added;
change_type("M ") ->
    indexed_modified;
change_type("M\t") ->
    indexed_modified;
change_type("D ") ->
    indexed_deleted;
change_type(" M") ->
    modified;
change_type(" D") ->
    deleted;
change_type("??") ->
    untracked.

status_changed_files(Repo, Prefix) ->
    [ {change_type([A,B]), filename:join(Prefix, F)}
      || [A,B,_ | F] <- string:tokens(oksh("git status --porcelain", [{cd, Repo}]), "\n") ].

add_files(Repo, Files, Prefix) ->
    sh("git add ~s", [string:join([filename:join(Prefix, F) || F <- Files], " ")], [{cd, Repo}]).

get_commits(Repo, Refs) ->
    RefStrs = join([ verstr(X) || X <- Refs], " "),
    lists:zip(Refs, string:tokens(oksh("git rev-parse ~s", [RefStrs], [{cd, Repo}]), "\n")).

get_reachable_tags(Repo, Tags) ->
    Commits = log_commits(Repo),
    get_reachable_tags(Repo, Tags, Commits).

get_reachable_tags(Repo, Tags, Commits) ->
    TagCommits = lists:zip(Tags, get_commits(Repo, Tags)),
    [ T || {T,{_, C}} <- TagCommits, lists:member(C, Commits) ].


diff_names(Repo, A, B, Prefix) when is_list(A),
                                    is_list(B),
                                    is_list(Prefix) ->
    Output = sh("git diff --name-status ~s ~s", [A, B], [{cd, Repo}]),
    [ {change_type([XA,XB]), filename:join(Prefix, F)}
      || [XA,XB | F] <- string:tokens(Output, "\n") ];
diff_names(Repo, A, B, Prefix) when is_list(Prefix) ->
    diff_names(Repo, verstr(A), verstr(B), Prefix).

verstr(A) when is_list(A) ->
    A;
verstr(A) when is_record(A, semver) ->
    semver:to_tag(A).

sh(Cmd, Opts) ->
    sh:sh(Cmd, [{use_stdout, false}, return_on_error] ++ Opts).
sh(Cmd, Args, Opts) ->
    sh:sh(Cmd, Args, [{use_stdout, false}, return_on_error] ++ Opts).

oksh(Cmd, Opts) ->
    {ok, Rep} = sh(Cmd, Opts),
    Rep.
oksh(Cmd, Args, Opts) ->
    {ok, Rep} = sh(Cmd, Args, Opts),
    Rep.
