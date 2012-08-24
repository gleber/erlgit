-module(git).

-export([is_repo_dirty/0,
         changed_files/0,
         log_commits/0,
         describe/0, semver/0,
         head/0,

         get_all_version_tags/0, get_all_version_tags_commits/0,
         get_reachable_versions/0,

         diff/2,

         add_files/1,
         commit/1, amend_changes/0,
         tag/1,
         reset_hard/1
        ]).

-import(git_utils, [exec/1, exec/2, exec/3, strip/1, join/2]).

-include_lib("erlsemver/include/semver.hrl").

%% =============================================================================
%%
%% API
%%
%% =============================================================================

is_repo_dirty() ->
    case exec("git status --porcelain | egrep -v \"^\\?\\?\"", [], true) of
        "" ->
            false;
        _ ->
            true
    end.

changed_files() ->
    changed_files(".").

add_files(Files) ->
    add_files(Files, ".").


get_all_version_tags() ->
    lists:sort([ semver:from_str(V) || [$v | V ] <- string:tokens(os:cmd("git tag"), "\n") ]).

get_all_version_tags_commits() ->
    get_tags_commits(get_all_version_tags()).

get_reachable_versions() ->
    Tags = get_all_version_tags(),
    get_reachable_tags(Tags).


commit(Msg) ->
    exec("git commit -m \"~s\"", [Msg]).

amend_changes() ->
    exec("git show HEAD --pretty=%s%n%n%b --summary | git commit -F - --amend").

tag(Ver) ->
    exec("git tag -f v~s", [semver:to_str(Ver)]),
    Ver.

log_commits() ->
    string:tokens(os:cmd("git log --format=\"%H\""), "\n").

describe() ->
    describe_tags().

describe_tags() ->
    strip(exec("git describe --tags", [], true)).

semver() ->
    semver:from_git_describe(describe_tags()).

diff(A, B) ->
    diff(A, B, ".").

head() ->
    strip(exec("git rev-parse HEAD", [], true)).

reset_hard(#semver{} = Ver) ->
    reset_hard(semver:to_tag(Ver));
reset_hard(Commit) ->
    strip(exec("git reset --hard ~s", [Commit])).

%% =============================================================================
%%
%% Internal
%%
%% =============================================================================

change_type("M ") ->
    indexed_modified;
change_type("D ") ->
    indexed_deleted;
change_type(" M") ->
    modified;
change_type("M\t") ->
    modified;
change_type("A\t") ->
    added;
change_type(" D") ->
    deleted;
change_type("??") ->
    untracked.

changed_files(Prefix) ->
    [ {change_type([A,B]), filename:join(Prefix, F)} || [A,B,_ | F] <- string:tokens(os:cmd("git status --porcelain"), "\n") ].

add_files(Files, Prefix) ->
    exec("git add ~s", [string:join([filename:join(Prefix, F) || F <- Files], " ")]).

get_tags_commits(Tags0) ->
    Tags = [ semver:to_tag(X) || X <- Tags0],
    TagStrs = join(Tags, " "),
    string:tokens(exec("git rev-parse ~s", [TagStrs], true), "\n").

get_reachable_tags(Tags) ->
    Commits = log_commits(),
    get_reachable_tags(Tags, Commits).

get_reachable_tags(Tags, Commits) ->
    TagCommits = lists:zip(Tags, get_tags_commits(Tags)),
    [ T || {T,C} <- TagCommits, lists:member(C, Commits) ].


diff(A, B, Prefix) when is_record(A, semver),
                        is_list(Prefix) ->
    diff(semver:to_tag(A), B, Prefix);
diff(A, B, Prefix) when is_record(B, semver),
                        is_list(Prefix) ->
    diff(A, semver:to_tag(B), Prefix);
diff(A, B, Prefix) when is_list(A),
                        is_list(B),
                        is_list(Prefix) ->
    [ {change_type([XA,XB]), filename:join(Prefix, F)} || [XA,XB,_ | F] <- string:tokens(exec("git diff --name-status ~s ~s", [A, B], true), "\n") ].

