-module(gin_example).

-compile({parse_transform, gin}).
-compile(export_all).



before_check_fun(X) when X =:= 1 orelse X =:= 2 orelse X =:= 3 ->
    true;
before_check_fun(_X) ->
    false.


after_check_fun(X) when in(X, [1, 2, 3]) ->
    true;
after_check_fun(_X) ->
    false.


wtf_fun(X, B) when in(X, [0, byte_size(B) > 10]) ->
    ok;
wtf_fun(_X, _B) ->
    {error, badarg}.


after_check_case(X) ->
    case X of 
        X when in(X, [1, 2, 3]) ->
            true;
        _ -> 
            false
    end.


after_check_if(X) ->
    if 
        in(X, [1, 2, 3]) ->
            true;
        true -> 
            false
    end.


-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

after_check_fun_test_() ->
    [ ?_assert(after_check_fun(1))
    , ?_assert(after_check_fun(2))
    , ?_assert(after_check_fun(3))
    , ?_assertNot(after_check_fun(4))
    ].

-endif.
