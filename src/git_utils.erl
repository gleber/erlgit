-module(git_utils).

-export([fformat/2,
         
         join/2,
         strip/1,

         mkdir/1
        ]).

fformat(Msg, Args) ->
    lists:flatten(io_lib:format(Msg, Args)).

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
            erlang:error(enoent)
    end.
