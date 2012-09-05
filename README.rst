Guard in
========

It is a tiny parse transform.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/mad-cocktail/gin.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/mad-cocktail/gin


.. code-block:: erlang

    > in(1, [1]).
    true
    > numeric_in(1, [1]).
    true
    > in(1, [1.0]).
    false
    > numeric_in(1, [1.0]).
    true
    > numeric_in({1}, [{1.0}]).
    true

Example 1
---------

Before:

.. code-block:: erlang

    ...
     case ux_unidata_parser:split($;, Data) of
     [Code, Form, Props] when (Form=="NFC_QC" orelse Form=="NFKC_QC"
                        orelse Form=="NFD_QC" orelse Form=="NFKD_QC")
                          and (Props=="N"
                        orelse Props=="Y"
                        orelse Props=="M") ->
     Atom = list_to_atom(string:to_lower(Props)),
    ...

After:

.. code-block:: erlang

     -compile({parse_transform, gin}).
    ...
     case ux_unidata_parser:split($;, Data) of
     [Code, Form, Props] when in(Form, ["NFC_QC", "NFKC_QC", "NFD_QC", "NFKD_QC"])
                          and in(Props, ["N", "Y", "M"]) ->
     Atom = list_to_atom(string:to_lower(Props)),
    ...

Example 2
---------

Before:

.. code-block:: erlang

    maybe_change_depth(Depth, Char) when Char =:= $)
                                         orelse Char =:= $]
                                         orelse Char =:= $} ->
      Depth - 1;
    maybe_change_depth(Depth, Char) when Char =:= $(
                                         orelse Char =:= $[
                                         orelse Char =:= ${ ->
      Depth + 1;
    maybe_change_depth(Depth, _) ->
      Depth.

After:

.. code-block:: erlang

     -compile({parse_transform, gin}).

    maybe_change_depth(Depth, Char) when in(Char, ")]}") ->
      Depth - 1;
    maybe_change_depth(Depth, Char) when in(Char, "([{") ->
      Depth + 1;
    maybe_change_depth(Depth, _) ->
      Depth.


Example 3
---------

From `here <https://github.com/Eonblast/Emysql/blob/master/src/emysql_tcp.erl>`_.

Before:

.. code-block:: erlang

    type_cast_row_data(Data, #field{type=Type})
    when Type == ?FIELD_TYPE_VARCHAR;
    Type == ?FIELD_TYPE_TINY_BLOB;
    Type == ?FIELD_TYPE_MEDIUM_BLOB;
    Type == ?FIELD_TYPE_LONG_BLOB;
    Type == ?FIELD_TYPE_BLOB;
    Type == ?FIELD_TYPE_VAR_STRING;
    Type == ?FIELD_TYPE_STRING ->
    Data;

    type_cast_row_data(Data, #field{type=Type})
    when Type == ?FIELD_TYPE_TINY;
    Type == ?FIELD_TYPE_SHORT;
    Type == ?FIELD_TYPE_LONG;
    Type == ?FIELD_TYPE_LONGLONG;
    Type == ?FIELD_TYPE_INT24;
    Type == ?FIELD_TYPE_YEAR ->
    list_to_integer(binary_to_list(Data));

    ...


Type cannot be a floated value, use ``=:=`` for comparation.

After:

.. code-block:: erlang

    type_cast_row_data(Data, #field{type=Type}) when
        in(Type, [?FIELD_TYPE_VARCHAR,      ?FIELD_TYPE_TINY_BLOB, 
                  ?FIELD_TYPE_MEDIUM_BLOB,  ?FIELD_TYPE_LONG_BLOB, 
                  ?FIELD_TYPE_BLOB,         ?FIELD_TYPE_VAR_STRING, 
                  ?FIELD_TYPE_STRING]) ->
    Data;

    type_cast_row_data(Data, #field{type=Type}) when 
        in(Type, [?FIELD_TYPE_TINY,     ?FIELD_TYPE_SHORT,  ?FIELD_TYPE_LONG,
                  ?FIELD_TYPE_LONGLONG, ?FIELD_TYPE_INT24,  ?FIELD_TYPE_YEAR] ->
    list_to_integer(binary_to_list(Data));

    ...


Example 4
---------

Before:

.. code-block:: erlang

    otp_7198_scan(<<>>, TokAcc) ->
            lists:reverse(['$thats_all_folks$' | TokAcc]);

    otp_7198_scan(<<D, Z, Rest/binary>>, TokAcc) when
                            (D =:= $D orelse D =:= $d) and
                            ((Z =:= $\s) or (Z =:= $() or (Z =:= $))) ->
            otp_7198_scan(<<Z, Rest/binary>>, ['AND' | TokAcc]);

    otp_7198_scan(<<D>>, TokAcc) when
                            (D =:= $D) or (D =:= $d) ->
            otp_7198_scan(<<>>, ['AND' | TokAcc]);

    otp_7198_scan(<<N, Z, Rest/binary>>, TokAcc) when
                            (N =:= $N orelse N =:= $n) and
                            ((Z =:= $\s) or (Z =:= $() or (Z =:= $))) ->
            otp_7198_scan(<<Z, Rest/binary>>, ['NOT' | TokAcc]);

    otp_7198_scan(<<C, Rest/binary>>, TokAcc) when
                                    (C >= $A) and (C =< $Z);
                                    (C >= $a) and (C =< $z);
                                    (C >= $0) and (C =< $9) ->
            case Rest of
                    <<$:, R/binary>> ->
                            otp_7198_scan(R, [{'FIELD', C} | TokAcc]);
                    _ ->
                            otp_7198_scan(Rest, [{'KEYWORD', C} | TokAcc])
            end.
    ...

After:

.. code-block:: erlang

     -compile({parse_transform, gin}).

    otp_7198_scan(<<>>, TokAcc) ->
        lists:reverse(['$thats_all_folks$' | TokAcc]);

    otp_7198_scan(<<D, Z, Rest/binary>>, TokAcc)
        when in(D, "Dd") and in(Z, "\s()") ->
        otp_7198_scan(<<Z, Rest/binary>>, ['AND' | TokAcc]);

    otp_7198_scan(<<D>>, TokAcc) when in(D, "Dd") ->
        otp_7198_scan(<<>>, ['AND' | TokAcc]);

    otp_7198_scan(<<N, Z, Rest/binary>>, TokAcc)
        when in(N, "Nn") and in(Z, "\s()") ->
        otp_7198_scan(<<Z, Rest/binary>>, ['NOT' | TokAcc]);

    otp_7198_scan(<<C, Rest/binary>>, TokAcc)
        when beetween(C, $A, $Z); beetween(C, $a, $z); beetween(C, $0, $9) ->
        case Rest of
            <<$:, R/binary>> ->
                otp_7198_scan(R, [{'FIELD', C} | TokAcc]);
            _ ->
                otp_7198_scan(Rest, [{'KEYWORD', C} | TokAcc])
        end.

    ...

We used ``beetween(Subject, From, To)`` here. It emalates the next
macros:

.. code-block:: erlang

    -define(BEETWEEN(S, S, E), (((C) >= (S)) andalso ((C) =< (E))).

This code is valid:

.. code-block:: erlang

    valid(X, Y) when in(X, [1,2,3,Y]) -> ok.

This code is invalid:

.. code-block:: erlang

    invalid(X, Y) when in(X, Y) -> error.
