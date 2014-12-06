-module(jlib_SUITE).
-compile([export_all]).

all() ->
    [jid_equality_tests].

init_per_suite(C) ->
    stringprep:start(),
    C.

s2j(S) -> jlib:binary_to_jid(list_to_binary(S)).

jid_equality_tests(_) ->
    true = jlib:are_equal_jids(s2j("hello@world.com"), s2j("hello@world.com")),
    true = jlib:are_equal_jids(s2j("hello@world.com/1"), s2j("hello@world.com/1")),
    false = jlib:are_equal_jids(s2j("hello@world.com"), s2j("bye@world.com")),
    false = jlib:are_equal_jids(s2j("hello@world.com"), s2j("hello@world.com/foo")).

