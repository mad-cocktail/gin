-module(gin).
-export([parse_transform/2]).

in_trans(AppNode) ->
    Pos = erl_syntax:get_pos(AppNode),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, ListForm] = erl_syntax:application_arguments(AppNode),
    %% Extract the list of the valid values.
    Elems = erl_syntax:list_elements(ListForm),
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
    

parse_transform(Forms, _Options) ->
    F = local_function(in, 2, fun in_trans/1),
    X = [postorder(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


postorder(F, Form) ->
    NewTree =
    case erl_syntax:subtrees(Form) of
    [] ->
        Form;
    List ->
        Groups = [handle_group(F, Group) || Group <- List],
        Tree2 = erl_syntax:update_tree(Form, Groups),
        Form2 = erl_syntax:revert(Tree2),
        Form2
    end,
    F(NewTree).


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
        
    
handle_group(F, Group) ->
    [postorder(F, Subtree) || Subtree <- Group].
