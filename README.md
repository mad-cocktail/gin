Guard in
========

It is a tiny parse transform.

__License__: MIT

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))

[![Build Status](https://secure.travis-ci.org/freeakk/gin.png?branch=master)](http://travis-ci.org/freeakk/gin)

Example 1
---------

Before:

```erlang
...
 case ux_unidata_parser:split($;, Data) of
 [Code, Form, Props] when (Form=="NFC_QC" orelse Form=="NFKC_QC"
                    orelse Form=="NFD_QC" orelse Form=="NFKD_QC")
                      and (Props=="N"
                    orelse Props=="Y"
                    orelse Props=="M") ->
 Atom = list_to_atom(string:to_lower(Props)),
...
```

After:

```erlang
 -compile({parse_transform, gin}).
...
 case ux_unidata_parser:split($;, Data) of
 [Code, Form, Props] when in(Form, ["NFC_QC", "NFKC_QC", "NFD_QC", "NFKD_QC"])
                      and in(Props, ["N", "Y", "M"]) ->
 Atom = list_to_atom(string:to_lower(Props)),
...
```


Example 2
---------

Before:

```erlang
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
```

After:

```erlang
 -compile({parse_transform, gin}).

maybe_change_depth(Depth, Char) when in(Char, ")]}") ->
  Depth - 1;
maybe_change_depth(Depth, Char) when in(Char, "([{") ->
  Depth + 1;
maybe_change_depth(Depth, _) ->
  Depth.
```


Example 3
---------

Before:

```erlang
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
...
```

After:

```erlang
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
...
```


This code is valid:

```erlang
valid(X, Y) when in(X, [1,2,3,Y]) -> ok.
```

This code is invalid:

```erlang
invalid(X, Y) when in(X, Y) -> error.
```
