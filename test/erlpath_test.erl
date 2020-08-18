-module(erlpath_test).

-author('fangbo.su@outlook.com').

-include_lib("eunit/include/eunit.hrl").

-import(erlpath, [
    parse/1,
    lookup/2,
    lookup/3,
    update/3,
    update/4,
    delete/2,
    delete/3
]).

-export([
    get/2
]).

setup() ->
    ok.

teardown(_) ->
    ok.

%%%===================================================================
%%% TEST
%%%===================================================================

get(a, 1) ->
    [{b,[[{i,1},{v,100},{s,2}],[{i,2},{v,200},{s,4}]]}, {c,[[{i,1},{v,300},{s,5}],[{i,2},{v,400},{s,7}]]}];
get(a, 2) ->
    [{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]}];
get(a, 3) ->
    [{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]},c].

-dialyzer({nowarn_function, parse_test_/0}).
parse_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            ?_assertEqual({ok,[{elem,a}]}, parse("a")),
            ?_assertEqual({ok,[{elem,a},{elem,b}]}, parse("a.b")),
            ?_assertEqual({ok,[{elem,a},{elem,b},{elem,c}]}, parse("a.b.c")),
            ?_assertEqual({ok,[{elem,a},{elem,b},{elem,c},{attr,{i,1}}]}, parse("a.b.c[i=1]")),
            ?_assertEqual({ok,[{elem,a},{elem,b},{elem,c},{attr,{i,1}},{elem,d}]}, parse("a.b.c[i=1].d")),
            ?_assertEqual({ok,[{elem,a},{elem,b},{elem,c},{attr,{i,1}},{elem,d},{elem,e}]}, parse("a.b.c[i=1].d.e")),

            ?_assertEqual(error, element(1, parse("A"))),
            ?_assertEqual(error, element(1, parse("A.a"))),
            ?_assertEqual(error, element(1, parse("a.A"))),
            ?_assertEqual(error, element(1, parse("a.b[i=A]"))),
            ?_assertEqual(error, element(1, parse("a.b[i=1].A"))),
            ?_assertEqual(error, element(1, parse("a.b[i=1].c.A"))),

            ?_assertEqual(error, element(1, parse("[i=1].c"))),
            ?_assertEqual(error, element(1, parse("[i].c"))),
            ?_assertEqual(error, element(1, parse("a[i].c"))),
            ?_assertEqual(error, element(1, parse("a[i.c"))),
            ?_assertEqual(error, element(1, parse("a[i=1.c")))
        ]
    }.

-dialyzer({nowarn_function, lookup_test_/0}).
lookup_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun lookup_t_001/0,
            fun lookup_t_002/0,
            fun lookup_t_003/0
        ]
    }.

lookup_t_001() ->
    ?assertEqual({ok, 100}, lookup("a.b[i=1].v", {?MODULE, get, [1]})).

lookup_t_002() ->
    ?assertEqual({ok,[{i,1},{v,100},{s,2}]}, lookup("a.b[i=1]", {?MODULE, get, [1]})).

lookup_t_003() ->
    Value = get(a,1),
    Result = lookup("a.b", {value, Value}, []),
    ?assertEqual({ok,[[{i,1},{v,100},{s,2}],[{i,2},{v,200},{s,4}]]}, Result).

-dialyzer({nowarn_function, update_test_/0}).
update_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun update_t_001/0,
            fun update_t_002/0,
            fun update_t_003/0,
            fun update_t_004/0,
            fun update_t_005/0,
            fun update_t_006/0,
            fun update_t_007/0,
            fun update_t_008/0,
            fun update_t_009/0,
            fun update_t_010/0,
            fun update_t_011/0
        ]
    }.

update_t_001() ->
    {ok, a, Result} = update("a.b[i=1].c", 103, {?MODULE, get, [2]}),
    ?assertEqual([{b,[[{i,1},c,{c,103}],[{i,2},c,{c,3}]]}], Result).

update_t_002() ->
    {ok, a, Result} = update("a.b[i=1].c", ccc, {mfa, {?MODULE, get, [2]}}, [{property, atom}]),
    ?assertEqual([{b,[[{i,1},ccc,{c,3}],[{i,2},c,{c,3}]]}], Result).

update_t_003() ->
    Value = get(a,2),
    {ok, a, Result} = update("a.b[i=1].c", ccc, {value, Value}, [{property, both}]),
    ?assertEqual([{b,[[{i,1},ccc,{c,ccc}],[{i,2},c,{c,3}]]}], Result).

update_t_004() ->
    Value = get(a,2),
    {ok, a, Result} = update("a.b[i=1]", c, {value, Value}, []),
    ?assertEqual([{b,[c,[{i,2},c,{c,3}]]}], Result).

update_t_005() ->
    {ok, a, Result} = update("a.b", ccc, {?MODULE, get, [2]}),
    ?assertEqual([{b,ccc}], Result).

update_t_006() ->
    {ok, a, Result} = update("a", ccc, {?MODULE, get, [2]}),
    ?assertEqual(ccc, Result).

update_t_007() ->
    {ok, a, Result} = update("a.b[i=1].d", 103, {?MODULE, get, [2]}),
    ?assertEqual([{b,[[{d,103},{i,1},c,{c,3}],[{i,2},c,{c,3}]]}], Result).

update_t_008() ->
    {ok, a, Result} = update("a.b[i=1].d", 103, {mfa,{?MODULE, get, [2]}}, [{property, both}]),
    ?assertEqual([{b,[[d,{d,103},{i,1},c,{c,3}],[{i,2},c,{c,3}]]}], Result).

update_t_009() ->
    {ok, a, Result} = update("a.b[i=1].d", 103, {mfa,{?MODULE, get, [2]}}, [{property, both},{upsert,false}]),
    ?assertEqual([{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]}], Result).

update_t_010() ->
    {ok, a, Result} = update("a.b[i=0].d", 103, {mfa,{?MODULE, get, [2]}}, []),
    ?assertEqual([{b,[[{d,103},{i,0}],[{i,1},c,{c,3}],[{i,2},c,{c,3}]]}], Result).

update_t_011() ->
    Result = update("a.b[i=0].d", 103, {mfa,{?MODULE, get, [2]}}, [{upsert,false}]),
    ?assertEqual(error, element(1, Result)).

-dialyzer({nowarn_function, delete_test_/0}).
delete_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun delete_t_001/0,
            fun delete_t_002/0,
            fun delete_t_003/0,
            fun delete_t_004/0,
            fun delete_t_005/0,
            fun delete_t_006/0,
            fun delete_t_007/0,
            fun delete_t_008/0,
            fun delete_t_009/0
        ]
    }.

delete_t_001() ->
    {ok, a, Result} = delete("a.b[i=1].c", {?MODULE, get, [3]}),
    ?assertEqual([{b,[[{i,1}],[{i,2},c,{c,3}]]},c], Result).

delete_t_002() ->
    {ok, a, Result} = delete("a.b[i=1].c", {mfa, {?MODULE, get, [3]}}, [{property, atom}]),
    ?assertEqual([{b,[[{i,1},{c,3}],[{i,2},c,{c,3}]]},c], Result).

delete_t_003() ->
    Value = get(a, 3),
    {ok, a, Result} = delete("a.b[i=1].c", {value, Value}, [{property, tuple}]),
    ?assertEqual([{b,[[{i,1},c],[{i,2},c,{c,3}]]},c], Result).

delete_t_004() ->
    Value = get(a, 3),
    {ok, a, Result} = delete("a.b[i=1]", {value, Value}, []),
    ?assertEqual([{b,[[{i,2},c,{c,3}]]},c], Result).

delete_t_005() ->
    {ok, a, Result} = delete("a.b", {?MODULE, get, [3]}),
    ?assertEqual([c], Result).

delete_t_006() ->
    {ok, a, Result} = delete("a", {?MODULE, get, [3]}),
    ?assertEqual([], Result).

delete_t_007() ->
    {ok, a, Result} = delete("a.b[i=1].d", {?MODULE, get, [3]}),
    ?assertEqual([{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]},c], Result).

delete_t_008() ->
    {ok, a, Result} = delete("a.b[i=3]", {?MODULE, get, [3]}),
    ?assertEqual([{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]},c], Result).

delete_t_009() ->
    {ok, a, Result} = delete("a.d", {?MODULE, get, [3]}),
    ?assertEqual([{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]},c], Result).
