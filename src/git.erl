-module(git).

-export([download/2, download/3,

         init/1,
         clone/2, clone/3,
         fetch/1,
         checkout/2, checkout/3,
         remote/1,
         remote/2, remote/3,

         status_is_detached/1,
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

-export([clone_cmd/3,
         fetch_cmd/1,
         checkout_cmd/3]).

-type option() :: 'no_checkout' | 'quite' | 'force'.
-type repo()   :: url() | dir().
-type url()    :: string().
-type dir()    :: file:filename().
-type head()   :: string().
-type branch() :: string().
-type tag()    :: string().
-type cid()    :: string().
-type remote() :: string().
-type ref()    :: head() | branch() | tag() | remote() | cid().
-type lref()   :: {'branch', branch()} | {'tag', tag()} | ref().
-type ref_type() :: 'head' | 'tag' | 'remote' | 'cid' | 'HEAD'.

-type change_type() :: indexed_added | indexed_modified | indexed_deleted | modified | deleted | untracked.


-import(git_utils, [fformat/2, strip/1, join/2]).

-include_lib("erlsemver/include/semver.hrl").

%% =============================================================================
%%
%% API
%%
%% =============================================================================

-spec download(url(), dir()) -> {'ok', string()} | {'error', term()}.
download(Url, Dir) ->
    clone(Url, Dir).

-spec download(url(), dir(), lref()) -> {'ok', string()} | {'error', term()}.
download(Url, Dir, "") ->
    download(Url, Dir, {branch, "HEAD"});
download(Url, Dir, {branch, Branch}) ->
    {ok, _} = clone(Url, Dir, [no_checkout]),
    checkout(Dir, fformat("origin/~s", Branch));
download(Url, Dir, {tag, Tag}) ->
    {ok, _} = clone(Url, Dir, [no_checkout]),
    checkout(Dir, Tag);
download(Url, Dir, Rev) ->
    {ok, _} = clone(Url, Dir, [no_checkout]),
    checkout(Dir, Rev).

%%
%% init empty project
-spec init(dir()) -> ok | {error, any()}.

init(RepoDir) ->
    ok = filelib:ensure_dir(filename:join([RepoDir, ".git"])),
    sh(init_cmd(RepoDir), [{cd, RepoDir}]).

init_cmd(_RepoDir) ->
    "git init".

%% @throws {unable_to_clone, Reason :: list()}>
-spec clone(url(), dir()) -> {'ok', string()} | {'error', term()}.
clone(RepoURL, RepoPath) ->
    clone(RepoURL, RepoPath, []).
-spec clone(url(), dir(), [option()]) -> {'ok', string()} | {'error', term()}.
clone(RepoURL, RepoPath, Opts) ->
    ok = filelib:ensure_dir(RepoPath),
    sh(clone_cmd(RepoURL, RepoPath, Opts), []).

clone_cmd(RepoURL, RepoPath, Opts) ->
    fformat("git clone ~s \"~s\" \"~s\"", [opts(Opts), RepoURL, RepoPath]).

%% @doc Fetches recent changes from repo.
%% @throws {unable_to_checkout, Reason}
-spec fetch(dir()) -> {'ok', string()} | {'error', term()}.
fetch(RepoDir) ->
    sh(fetch_cmd(RepoDir), [{cd, RepoDir}]).

fetch_cmd(_RepoDir) ->
    "git fetch".

%% @doc Tries to checkout to given commit.
%% @throws {unable_to_checkout, Reason}
-spec checkout(dir(), ref()) -> {'ok', string()} | {'error', term()}.
checkout(RepoDir, CommitID) ->
    checkout(RepoDir, CommitID, [quite, force]).
-spec checkout(dir(), ref(), [option()]) -> {'ok', string()} | {'error', term()}.
checkout(RepoDir, CommitID, Opts) ->
    sh(checkout_cmd(RepoDir, CommitID, Opts), [{cd, RepoDir}]).

checkout_cmd(_RepoDir, CommitID, Opts) ->
    fformat("git checkout ~s ~s", [opts(Opts), CommitID]).

%% @doc List of remote entities: git remote -v
%%
-spec(remote/1 :: (dir()) -> {ok, list()}).

remote(Repo) ->
     lists:usort(
          [string_to_remote(X) || 
               X <- string:tokens(oksh("git remote -v", [{cd, Repo}]), "\n")]
     ).

string_to_remote(X) ->
     [Name, Url, _] = string:tokens(X, "\t "),
     {Name, Url}.

%%
%% add git remote to existed repository
-spec(remote/2 :: (dir(), url()) -> ok | {error, any()}).
-spec(remote/3 :: (dir(), list(), url()) -> ok | {error, any()}).

remote(RepoDir, RepoURL) ->
    remote(RepoDir, "origin", RepoURL).

remote(RepoDir, RemoteName, RepoURL) ->
   sh(remote_add_cmd(RemoteName, RepoURL), [{cd, RepoDir}]).

remote_add_cmd(RemoteName, RepoURL) ->
    fformat("git remote add -t \\* -f ~s ~s", [RemoteName, RepoURL]).


-spec branch(dir()) -> {'ok', branch()} | 'detached'.
branch(Repo) ->
    case status_is_detached(Repo) of
        false ->
            Refs = refs(Repo),
            {value, {"HEAD", 'HEAD', H}, Refs2} = lists:keytake('HEAD', 2, Refs),
            B = [ N || {N, T, C} <- Refs2, T == head, C == H ],
            {ok, list_join(B, "; ")};
        true ->
            detached
    end.

-spec branches(repo()) -> [branch()].
branches(Repo) ->
    [ N || {N, T, _C} <- refs(Repo), T == head ].
-spec branches_commits(repo()) -> [{branch(), cid()}].
branches_commits(Repo) ->
    [ {N, C} || {N, T, C} <- refs(Repo), T == head ].

-spec refs(repo()) -> [{ref(), ref_type(), cid()}].
refs(Repo) ->
    Output = oksh(refs_cmd(Repo), []),
    lists:flatmap(fun(L) ->
                          case string:tokens(L, "\t ") of
                              ["DEBUG:"++_|_] ->
                                  [];
                              ["WARNING:"|_] ->
                                  [];
                              [Commit, Ref] ->
                                  {Type, Name} = case string:tokens(Ref, "/") of
                                                     ["refs", T | N] ->
                                                         {T, string:join(N, "/")};
                                                     ["HEAD"] ->
                                                         {"HEAD", "HEAD"}
                                                 end,
                                  [{Name, list_to_atom(string:strip(Type, right, $s)), Commit}]
                          end
                  end, string:tokens(Output, [13,10])).

refs_cmd(Repo) ->
    fformat("git ls-remote ~s", [Repo]).

-spec status_is_detached(dir()) -> boolean().
status_is_detached(Repo) ->
    case strip(oksh("git rev-parse --symbolic-full-name --abbrev-ref HEAD", [{cd, Repo}])) of
        "HEAD" ->
            true;
        _ ->
            false
    end.

-spec status_is_dirty(dir()) -> boolean().
status_is_dirty(Repo) ->
    not lists:all(fun({untracked, _}) -> true;
                     (_) -> false
                  end, status_changed_files(Repo)).

-spec status_changed_files(dir()) -> [{change_type(), file:filename()}].
status_changed_files(Repo) ->
    status_changed_files(Repo, ".").

-spec add_files(dir(), [file:filename()]) -> {'ok', string()} | {'error', term()}.
add_files(Repo, Files) ->
    add_files(Repo, Files, ".").

-spec remotes(repo()) -> [remote()].
remotes(Repo) ->
    [ N || {N, T, _C} <- refs(Repo), T == remote ].
-spec remotes_commits(repo()) -> [{remote(), cid()}].
remotes_commits(Repo) ->
    [ {N, C} || {N, T, C} <- refs(Repo), T == remote ].

-spec tags(repo()) -> [tag()].
tags(Repo) ->
    [ N || {N, T, _C} <- refs(Repo), T == tag ].
-spec tags_commits(repo()) -> [{tag(), cid()}].
tags_commits(Repo) ->
    [ {N, C} || {N, T, C} <- refs(Repo), T == tag ].

-spec version_tags(repo()) -> [semver:semver()].
version_tags(Repo) ->
    [ semver:from_str(V) || [$v | V ] <- tags(Repo) ].

-spec version_tags_commits(repo()) -> [{semver:semver(), cid()}].
version_tags_commits(Repo) ->
    get_commits(Repo, version_tags(Repo)).

-spec get_reachable_versions(repo()) -> [semver:semver()].
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
opts(Opts) ->
    lists:flatten(join(lists:map(fun opt/1, Opts), " ")).
opt(no_checkout) ->
    "-n";
opt(quite) ->
    "-q";
opt(force) ->
    "-f".
-spec change_type(string()) -> change_type().
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
change_type("MM") ->
    {several, [indexed_modified, modified]};
change_type(" D") ->
    deleted;
change_type("??") ->
    untracked.

status_changed_files(Repo, Prefix) ->
    Status = string:tokens(oksh("git status --porcelain", [{cd, Repo}]), "\n"),
    Changed = [ construct_change(change_type([A,B]), filename:join(Prefix, F)) ||
                [A,B,_ | F] <- Status ],
    lists:flatten(Changed).

construct_change({several, ChangeTypes}, Filename) ->
    [ construct_change(ChangeType, Filename) || ChangeType <- ChangeTypes ];
construct_change(ChangeType, Filename) ->
    {ChangeType, Filename}.


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

list_join([H], _Separator) -> H;
list_join([H | T], Separator) ->
        [H | [[Separator, S] || S <- T]].
