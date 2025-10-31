-module(lispy@erlang_ffi).
-export([erlang_apply/3, value_to_erlang/1, erlang_to_value/1]).

%% Call Erlang function dynamically: Module:Function(Args)
%% Returns {ok, Result} or {error, {erlang_error, ReasonString}}
erlang_apply(Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
    try
        ErlangArgs = [value_to_erlang(Arg) || Arg <- Args],
        Result = apply(Module, Function, ErlangArgs),
        {ok, erlang_to_value(Result)}
    catch
        error:Reason ->
            ReasonStr = format_error(Reason),
            {error, {erlang_error, ReasonStr}};
        throw:Reason ->
            ReasonStr = format_error(Reason),
            {error, {erlang_error, ReasonStr}};
        exit:Reason ->
            ReasonStr = format_error(Reason),
            {error, {erlang_error, ReasonStr}}
    end;
erlang_apply(_, _, _) ->
    {error, invalid_arguments}.

%% Format error reason as binary string
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%% Convert Lisp value to Erlang term
value_to_erlang({integer, N}) -> N;
value_to_erlang({double, F}) -> F;
value_to_erlang({string, S}) -> S;
value_to_erlang({symbol, Name}) -> binary_to_atom(Name, utf8);
value_to_erlang({boolean, true}) -> true;
value_to_erlang({boolean, false}) -> false;
value_to_erlang({atom, A}) -> A;
value_to_erlang({cons, Car, Cdr}) ->
    [value_to_erlang(Car) | value_to_erlang(Cdr)];
value_to_erlang(nil) -> [];
value_to_erlang({dict, Dict}) ->
    maps:from_list([{value_to_erlang(K), value_to_erlang(V)} || {K, V} <- Dict]);
value_to_erlang(Other) -> Other.

%% Convert Erlang term back to Lisp value
erlang_to_value(N) when is_integer(N) -> {integer, N};
erlang_to_value(F) when is_float(F) -> {double, F};
erlang_to_value(S) when is_binary(S) -> {string, S};
erlang_to_value(true) -> {boolean, true};
erlang_to_value(false) -> {boolean, false};
erlang_to_value(A) when is_atom(A) -> {atom, A};
erlang_to_value([]) -> nil;
erlang_to_value([H|T]) -> {cons, erlang_to_value(H), erlang_to_value(T)};
erlang_to_value(M) when is_map(M) ->
    Dict = [{erlang_to_value(K), erlang_to_value(V)} || {K, V} <- maps:to_list(M)],
    {dict, Dict};
erlang_to_value(T) when is_tuple(T) ->
    List = tuple_to_list(T),
    list_to_cons([erlang_to_value(E) || E <- List]);
erlang_to_value(Other) ->
    {string, list_to_binary(io_lib:format("~p", [Other]))}.

%% Helper to convert list to cons cells
list_to_cons([]) -> nil;
list_to_cons([H|T]) -> {cons, H, list_to_cons(T)}.
