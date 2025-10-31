-module(lispy@io_ffi).
-export([get_line/1]).

%% Simple wrapper around io:get_line/1
%% Returns {ok, Binary} or {error, nil}
get_line(Prompt) when is_binary(Prompt) ->
    PromptStr = binary_to_list(Prompt),
    case io:get_line(PromptStr) of
        eof ->
            {error, nil};
        {error, _} ->
            {error, nil};
        Line when is_list(Line) ->
            {ok, list_to_binary(Line)};
        Line when is_binary(Line) ->
            {ok, Line}
    end;
get_line(_) ->
    {error, nil}.
