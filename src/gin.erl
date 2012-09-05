-module(gin).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    F1 = local_function(numeric_in, 2, in_transform('==')),
    F2 = local_function(in, 2, in_transform('=:=')),
    F3 = local_function(beetween, 3, fun beetween_transform/1),
    F  = oneof_function([F1, F2, F3, fun erl_syntax:revert/1]),
    X = [erl_syntax_lib:map(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


%% ==================================================================
%% In
%% ==================================================================

%% It is curry for `in_transform'.
in_transform(Op) ->
    fun(Node) ->
        in_transform(Op, Node)
        end.

-spec in_transform(Op, Node) -> Node when
    Op :: '==' | '=:=',
    Node :: erl_syntax_lib:syntaxTree().

in_transform(Op, Node) ->
    Pos = erl_syntax:get_pos(Node),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, ListForm] = erl_syntax:application_arguments(Node),
    Elems =
        case erl_syntax:type(ListForm) of
        string ->
            Str = erl_syntax:string_value(ListForm),
            [erl_syntax:char(C) || C <- Str];
        list ->
            %% Extract the list of the valid values.
            erl_syntax:list_elements(ListForm)
        end,
    case Elems of
    [] ->
        %% Always `false'.
        New(erl_syntax:atom(false));
    
    _  ->
        EqOp = New(erl_syntax:operator(Op)),
        OrOp = New(erl_syntax:operator('orelse')),
        %% `X' is `Subject =:= Xs'.
        [X|Xs] = [New(erl_syntax:infix_expr(E, EqOp, SubjectForm)) || E <- Elems],
        F = fun(Right, Left) -> New(erl_syntax:infix_expr(Left, OrOp, Right)) end,
        GuardAST = New(erl_syntax:parentheses(lists:foldl(F, X, Xs))),
        erl_syntax:revert(GuardAST)
    end.


%% ==================================================================
%% Beetween
%% ==================================================================

beetween_transform(Node) ->
    Pos = erl_syntax:get_pos(Node),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, FromForm, ToForm] = 
        erl_syntax:application_arguments(Node),
    GtEqOp = New(erl_syntax:operator(greater(is_excluded(FromForm)))),
    LoEqOp = New(erl_syntax:operator(less(is_excluded(ToForm)))),
    AndOp  = New(erl_syntax:operator('andalso')),
    Exp1 = New(erl_syntax:infix_expr(SubjectForm, GtEqOp, clean_excluded(FromForm))),
    Exp2 = New(erl_syntax:infix_expr(SubjectForm, LoEqOp, clean_excluded(ToForm))),
    Exp3 = New(erl_syntax:infix_expr(Exp1, AndOp, Exp2)),
    GuardAST = New(erl_syntax:parentheses(Exp3)),
    erl_syntax:revert(GuardAST).

less(true)  -> '<';
less(false) -> '=<'.

greater(true)  -> '>';
greater(false) -> '>='.

is_excluded(Node) ->
    is_local_function(exclude, 1, Node).

clean_excluded(Node) ->
    case is_excluded(Node) of
        true ->  head(erl_syntax:application_arguments(Node));
        false -> Node
    end.

head([H|_]) -> H.




oneof_function(Fs) ->
    fun(Node) ->
        Apply = fun(F, N) -> F(N) end,
        lists:foldl(Apply, Node, Fs)
    end.


local_function(FunName, FunArity, TransFun) ->
    fun(Node) ->
        IsFun = is_local_function(FunName, FunArity, Node),
        if IsFun -> TransFun(Node);
            true -> Node
            end
        end.

is_local_function(FunName, FunArity, Node) -> 
    erl_syntax:type(Node) =:= application
        andalso always(Op = erl_syntax:application_operator(Node))
        andalso erl_syntax:type(Op) =:= atom
        andalso erl_syntax:atom_value(Op) =:= FunName
        andalso application_arity(Node) =:= FunArity.

always(_) -> true.


application_arity(Node) ->
    length(erl_syntax:application_arguments(Node)).
