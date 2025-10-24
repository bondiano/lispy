-module(lispy_ffi).
-export([read_line/1]).

read_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, nil};
        {error, _} -> {error, nil};
        Line when is_list(Line) -> {ok, list_to_binary(Line)};
        Line when is_binary(Line) -> {ok, Line}
    end.
