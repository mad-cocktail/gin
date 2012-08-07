-module(gin_tests).

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


as_string(Char) when in(Char, ")]}") -> 
    true;
as_string(_Char) -> 
    false.


wtf_fun(X, B) when in(X, [0, byte_size(B) > 10]) ->
    ok;
wtf_fun(_X, _B) ->
    {error, badarg}.


twice(X, Y) when in(X, [1,2,3]), in(Y, [5,6,7]) ->
    true;
twice(_X, _Y) ->
    false.

range_fun(X, Y) when beetween(X, 1, 3), beetween(Y, 5, 7) ->
    true;
range_fun(_X, _Y) ->
    false.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

after_check_fun_test_() ->
    [ ?_assert(after_check_fun(1))
    , ?_assert(after_check_fun(2))
    , ?_assert(after_check_fun(3))
    , ?_assertNot(after_check_fun(4))
    ].

after_check_if_test_() ->
    [ ?_assert(after_check_if(1))
    , ?_assert(after_check_if(2))
    , ?_assert(after_check_if(3))
    , ?_assertNot(after_check_if(4))
    ].

after_check_case_test_() ->
    [ ?_assert(after_check_case(1))
    , ?_assert(after_check_case(2))
    , ?_assert(after_check_case(3))
    , ?_assertNot(after_check_case(4))
    ].

as_string_fun_test_() ->
    [ ?_assert(as_string($)))
    , ?_assert(as_string($]))
    , ?_assert(as_string($}))
    , ?_assertNot(as_string($a))
    ].

twice_test_() ->
    [ ?_assert(twice(1, 5))
    , ?_assert(twice(2, 5))
    , ?_assert(twice(2, 5))
    , ?_assertNot(twice(5, 5))
    , ?_assertNot(twice(2, 0))
    , ?_assertNot(twice(2, 1))
    , ?_assertNot(twice(5, 1))
    , ?_assertNot(twice(0, 0))
    , ?_assertNot(twice(5, 1))
    ].


beetween_test_() ->
    [ ?_assert(range_fun(1, 5))
    , ?_assert(range_fun(2, 5))
    , ?_assert(range_fun(2, 5))
    , ?_assertNot(range_fun(5, 5))
    , ?_assertNot(range_fun(2, 0))
    , ?_assertNot(range_fun(2, 1))
    , ?_assertNot(range_fun(5, 1))
    , ?_assertNot(range_fun(0, 0))
    , ?_assertNot(range_fun(5, 1))
    ].

-endif.
