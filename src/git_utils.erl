-module(git_utils).

-export([join/2,
         strip/1,

         mkdir/1,
         exec/1,
         exec/2,
         exec/3,
         exec/4
        ]).

strip(Value)->
    re:replace(re:replace(Value, "^\\s*", ""), "\\s*$", "", [{return, list}]). %"

join([], _) ->
    [];
join([H], _S) ->
    [H];
join([H|T], S) ->
    [H, S | join(T, S)].


mkdir(Dir0) ->
    Dir = Dir0 ++ "/",
    ok = filelib:ensure_dir(Dir),
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            io:format("Failed to create dir ~s~n", [Dir]),
            erlang:error(enoent)
    end.

exec(Str) ->
    exec(Str, []).
exec(Str, Args) ->
    exec(Str, Args, false).

exec(Str, Args, X) ->
    exec(Str, Args, X, false).

exec(Str, Args, true, Out) ->
    Cmd = lists:flatten(io_lib:format(Str, Args)),
    exec0(Cmd, Out);
exec(Str, Args, _, Out) ->
    Cmd = lists:flatten(io_lib:format(Str, Args)),
    io:format("Executing '~s'~n", [Cmd]),
    exec0(Cmd, Out).


exec0(Cmd, false) ->
    os:cmd("sh -c '"++Cmd++"'");
exec0(Cmd, true)  ->
    exec0(Cmd, "   > ");
exec0(Cmd, Prefix) when is_list(Prefix) ->
    R0 = os:cmd("sh -c '"++Cmd++"'"),
    R = string:tokens(R0, "\n"),
    io:format([ [Prefix, X, "\n"] || X <- R ]).
