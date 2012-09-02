-module(gin).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    F1 = local_function(in, 2, fun in_transform/1),
    F2 = local_function(beetween, 3, fun beetween_transform/1),
    F  = oneof_function([F1, F2, fun erl_syntax:revert/1]),
    X = [erl_syntax_lib:map(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


in_transform(AppNode) ->
    Pos = erl_syntax:get_pos(AppNode),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, ListForm] = erl_syntax:application_arguments(AppNode),
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
        EqOp = New(erl_syntax:operator('=:=')),
        OrOp = New(erl_syntax:operator('orelse')),
        %% `X' is `Subject =:= Xs'.
        [X|Xs] = [New(erl_syntax:infix_expr(E, EqOp, SubjectForm)) || E <- Elems],
        F = fun(Right, Left) -> New(erl_syntax:infix_expr(Left, OrOp, Right)) end,
        GuardAST = New(erl_syntax:parentheses(lists:foldl(F, X, Xs))),
        erl_syntax:revert(GuardAST)
    end.


beetween_transform(AppNode) ->
    Pos = erl_syntax:get_pos(AppNode),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, FromForm, ToForm] = 
        erl_syntax:application_arguments(AppNode),
    GtEqOp = New(erl_syntax:operator('>=')),
    LoEqOp = New(erl_syntax:operator('=<')),
    AndOp  = New(erl_syntax:operator('andalso')),
    Exp1 = New(erl_syntax:infix_expr(SubjectForm, GtEqOp, FromForm)),
    Exp2 = New(erl_syntax:infix_expr(SubjectForm, LoEqOp, ToForm)),
    Exp3 = New(erl_syntax:infix_expr(Exp1, AndOp, Exp2)),
    GuardAST = New(erl_syntax:parentheses(Exp3)),
    erl_syntax:revert(GuardAST).


oneof_function(Fs) ->
    fun(Node) ->
        Apply = fun(F, N) -> F(N) end,
        lists:foldl(Apply, Node, Fs)
    end.


local_function(FunName, FunArity, TransFun) ->
    fun(Node) ->
        IsFun = erl_syntax:type(Node) =:= application
            andalso always(Op = erl_syntax:application_operator(Node))
            andalso erl_syntax:type(Op) =:= atom
            andalso erl_syntax:atom_value(Op) =:= FunName
            andalso application_arity(Node) =:= FunArity,
            
        if IsFun -> TransFun(Node);
            true -> Node
            end
        end.
        
always(_) -> true.


application_arity(AppNode) ->
    length(erl_syntax:application_arguments(AppNode)).
