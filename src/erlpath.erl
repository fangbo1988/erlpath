%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @author: fangbo.su@outlook.com

-module(erlpath).
-author('fangbo.su@outlook.com').

-behaviour(gen_server).

-export([
    lookup/2,
    lookup/3,
    update/3,
    update/4,
    update/5,
    delete/2,
    delete/3,
    delete/4,
    parse/1,
    parse/2
]).

-export([
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-define(get(Key, List), proplists:get_value(Key, List)).
-define(get(Key, List, Default), proplists:get_value(Key, List, Default)).
-define(has(Key, List), proplists:is_defined(Key, List)).
-define(delete(Key, List), proplists:delete(Key, List)).

-type element()  :: {elem, atom()} | {attr, {atom(), atom() | integer() | binary()}}.
-type elements() :: [element()].
-type property() :: atom() | {atom(), term()}.
-type properties() :: [property()].
-type emfa() :: {atom(), atom(), [term()]}.

-record(state, {}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}, hibernate}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse(binary() | string()) -> {ok, elements()} | {error, any()}.
parse(Path) when is_binary(Path) ->
    parse(binary_to_list(Path));
parse(Path) when is_list(Path) ->
    case lex(Path) of
        {ok, Tokens} ->
            parse_tokens(Tokens);
        {error, Reason} ->
            {error, Reason}
    end.

-spec lex(string()) -> {ok, [tuple()]} | {error, any()}.
lex(String) when is_list(String) ->
    case erlpath_lexer:string(String) of
        {ok, Tokens, _TokenLine} ->
            {ok, Tokens};
        {error, {ErrorLine, Mod, Reason}, _EndLine} ->
            {error, {illegal_path, ErrorLine, Mod:format_error(Reason)}}
    end.

-spec parse_tokens([tuple()]) -> {ok, elements()} | {error, any()}.
parse_tokens(Tokens) ->
    case erlpath_parser:parse(Tokens) of
        {ok, Result} ->
            {ok, Result};
        {error, {ErrorLine, Mod, Reason}} ->
            {error, {illegal_syntax, ErrorLine, Mod:format_error(Reason)}}
    end.

-spec lookup(binary() | string(), emfa()) -> {ok, any()} | {error, any()}.
lookup(Path, MFA) ->
    lookup(Path, {mfa, MFA}, []).

-spec lookup(binary() | string(), {mfa, emfa()} | {value, term()}, properties()) -> {ok, any()} | {error, any()}.
lookup(Path, MFAorValue, Opts) ->
    case parse(Path, Opts) of
        {ok, Elem, Elements} ->
            case get_root_value(Elem, MFAorValue) of
                {ok, ElemValue} ->
                    lookup2(Elements, Elem, ElemValue);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_root_value(atom(), {mfa, emfa()} | {value, term()}) -> {ok, term()} | {error, any()}.
get_root_value(Elem, {mfa, {M, F, A}}) ->
    case catch apply(M, F, [Elem]++A) of
        {'EXIT', Reason} ->
            {error, Reason};
        undefined ->
            {error, undefined};
        ElemValue ->
            {ok, ElemValue}
    end;
get_root_value(_Elem, {value, Value}) ->
    {ok, Value}.

-spec lookup2(elements(), atom(), any()) -> {ok, any()} | {error, any()}.
lookup2([], _Elem, Value) ->
    {ok, Value};
lookup2([{elem, Elem} | Rest], _ParentElem, ParentValue) ->
    case ?get(Elem, ParentValue, '__undefined__') of
        '__undefined__' -> {error, {elem_not_found, Elem}};
        ElemValue -> lookup2(Rest, Elem, ElemValue)
    end;
lookup2([{attr, {AttrName, AttrValue} = Attr} | Rest], ParentElem, ParentValue) when is_list(ParentValue) ->
    Search = lists:search(fun(Val) ->
        case ?get(AttrName, Val, '__undefined__') of
            '__undefined__' -> false;
            V -> V =:= AttrValue
        end
    end, ParentValue),
    case Search of
        false -> {error, {attr_not_found, Attr, ParentElem}};
        {value, Value} -> lookup2(Rest, ParentElem, Value)
    end;
lookup2([{attr, _Attr} | _Rest], ParentElem, _ParentValue) ->
    {error, {not_a_list, ParentElem}}.

-spec parse(binary() | string(), properties()) -> {ok, atom(), elements()} | {error, any()}.
parse(Path, Opts) ->
    case parse(Path) of
        {ok, Elements} ->
            [{elem, Elem} | Rest] = Elements,
            case ?has(hasRoot, Opts)of
                true -> {ok, Elem, Elements};
                false -> {ok, Elem, Rest}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec update(binary() | string(), any(), emfa()) -> {ok, any()} | {error, any()}.
update(Path, Value, MFA) ->
    update(Path, Value, {mfa, MFA}, []).

-spec update(binary() | string(), any(), {mfa, emfa()} | {value, term()}, properties()) -> {ok, atom(), any()} | {error, any()}.
update(Path, Value, MFAorValue, Opts) ->
    case parse(Path, Opts) of
        {ok, Elem, Elements} ->
            case get_root_value(Elem, MFAorValue) of
                {ok, []} ->
                    {error, {empty_list, MFAorValue, Elem}};
                {ok, ElemValue} ->
                    update2(Elements, Elem, ElemValue, Value, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec update2(elements(), atom() | tuple(), any(), any(), properties()) -> {ok, atom() | tuple(), any()} | {error, any()}.
update2([], ParentElem, _ParentValue, Value, _Opts) ->
    {ok, ParentElem, Value};
update2([{elem, Elem} | Rest], ParentElem, ParentValue, Value, Opts) when is_list(ParentValue) ->
    case ?get(Elem, ParentValue, '__undefined__') of
        '__undefined__' ->
            case ?get(upsert, Opts, true) of
                true ->
                    case ?get(property, Opts, tuple) of
                        both -> {ok, ParentElem, [Elem, {Elem, Value} | ParentValue]};
                        atom -> {ok, ParentElem, [Elem | ParentValue]};
                        tuple -> {ok, ParentElem, [{Elem, Value} | ParentValue]}
                    end;
                false ->
                    {ok, ParentElem, ParentValue}
            end;
        ElemValue ->
            case update2(Rest, Elem, ElemValue, Value, Opts) of
                {ok, _, Value} ->
                    NewParentValue = do_update(ParentValue, Elem, Value, Opts),
                    {ok, ParentElem, NewParentValue};
                {ok, _, NewElemValue} ->
                    ParentValueDel = ?delete(Elem, ParentValue),
                    {ok, ParentElem, [{Elem, NewElemValue} | ParentValueDel]};
                {error, Reason} ->
                    {error, Reason}
            end
    end;
update2([{elem, _Elem} | _Rest], ParentElem, _ParentValue, _Value, _Opts) ->
    {error, {not_a_list, ParentElem}};
update2([{attr, {AttrName, AttrValue} = Attr} | Rest], ParentElem, ParentValue, Value, Opts) when is_list(ParentValue) ->
    Search = lists:search(fun(Val) ->
        case ?get(AttrName, Val, '__undefined__') of
            '__undefined__' -> false;
            V -> V =:= AttrValue
        end
    end, ParentValue),
    case
        case Search of
            false ->
                case ?get(upsert, Opts, true) of
                    true -> {ok, [{AttrName, AttrValue}]};
                    false -> {error, {attr_not_found, Attr, ParentElem}}
                end;
            {value, Result} ->
                {ok, Result}
        end
    of
        {ok, ElemValue} ->
            ParentValueDel = ParentValue -- [ElemValue],
            case update2(Rest, {ParentElem, Attr} , ElemValue, Value, Opts) of
                {ok, _, Value} ->
                    {ok, ParentElem, [Value | ParentValueDel]};
                {ok, _, NewElemValue} ->
                    NewParentValue = [NewElemValue | ParentValueDel],
                    {ok, ParentElem, NewParentValue};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
update2([{attr, _Attr} | _Rest], ParentElem, _ParentValue, _Value, _Opts) ->
    {error, {not_a_list, ParentElem}}.

do_update(ParentValue, Elem, ElemValue, Opts) ->
    do_update(ParentValue, Elem, ElemValue, Opts, false, []).

do_update([], _Elem, _ElemValue, _Opts, true, Acc) ->
    lists:reverse(Acc);
do_update([], Elem, ElemValue, Opts, false, Acc) ->
    case ?get(upsert, Opts, true) of
        true ->
            case ?get(property, Opts, tuple) of
                both -> do_update([], Elem, ElemValue, Opts, true, [Elem, {Elem, ElemValue} | Acc]);
                atom -> do_update([], Elem, ElemValue, Opts, true, [Elem | Acc]);
                tuple -> do_update([], Elem, ElemValue, Opts, true, [{Elem, ElemValue} | Acc])
            end;
        false ->
            do_update([], Elem, ElemValue, Opts, true, Acc)
    end;
do_update([Elem | Rest], Elem, ElemValue, Opts, Did, Acc) ->
    case ?get(property, Opts, tuple) of
        both -> do_update(Rest, Elem, ElemValue, Opts, true, [ElemValue | Acc]);
        atom -> do_update(Rest, Elem, ElemValue, Opts, true, [ElemValue | Acc]);
        tuple -> do_update(Rest, Elem, ElemValue, Opts, Did, [Elem | Acc])
    end;
do_update([{Elem, Value} | Rest], Elem, ElemValue, Opts, Did, Acc) ->
    case ?get(property, Opts, tuple) of
        atom -> do_update(Rest, Elem, ElemValue, Opts, Did, [{Elem, Value} | Acc]);
        both -> do_update(Rest, Elem, ElemValue, Opts, true, [{Elem, ElemValue} | Acc]);
        tuple -> do_update(Rest, Elem, ElemValue, Opts, true, [{Elem, ElemValue} | Acc])
    end;
do_update([ElemOther | Rest], Elem, ElemValue, Opts, Did, Acc) ->
    do_update(Rest, Elem, ElemValue, Opts, Did, [ElemOther | Acc]).

-spec update(binary() | string(), any(), emfa(), emfa(), properties()) -> {ok, any()} | {error, any()}.
update(Path, Value, MFA, {M,F,A} = _MFASet, Opts) ->
    case update(Path, Value, {mfa, MFA}, Opts) of
        {ok, RootElem, RootElemValue} ->
            case catch apply(M, F, [RootElem,RootElemValue]++A) of
                {'EXIT', Reason} ->
                    {error, Reason};
                Result ->
                    {ok, Result}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(binary() | string(), emfa()) -> {ok, any()} | {error, any()}.
delete(Path, MFA) ->
    delete(Path, {mfa, MFA}, []).

-spec delete(binary() | string(), {mfa, emfa()} | {value, term()}, properties()) -> {ok, atom(), any()} | {error, any()}.
delete(Path, MFAorValue, Opts) ->
    case parse(Path, Opts) of
        {ok, Elem, Elements} ->
            case get_root_value(Elem, MFAorValue) of
                {ok, ElemValue} ->
                    delete2(Elements, Elem, ElemValue, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete2(elements(), atom() | tuple(), any(), properties()) -> {ok, atom() | tuple(), any()} | {error, any()}.
delete2([], Elem, _ElemValue, _Opts) ->
    {ok, Elem, []};
delete2([{elem, Elem} | Rest], ParentElem, ParentValue, Opts) when is_list(ParentValue) ->
    case ?get(Elem, ParentValue, '__undefined__') of
        '__undefined__' ->
            {ok, ParentElem, ParentValue};
        ElemValue ->
            case delete2(Rest, Elem, ElemValue, Opts) of
                {ok, _, []} ->
                    ParentValueDel = do_delete(ParentValue, Elem, Opts),
                    {ok, ParentElem, ParentValueDel};
                {ok, _, NewElemValue} ->
                    ParentValueDel = ?delete(Elem, ParentValue),
                    {ok, ParentElem, [{Elem, NewElemValue} | ParentValueDel]};
                {error, Reason} ->
                    {error, Reason}
            end
    end;
delete2([{elem, _Elem} | _Rest], ParentElem, _ParentValue, _Opts) ->
    {error, {not_a_list, ParentElem}};
delete2([{attr, {AttrName, AttrValue} = Attr} | Rest], ParentElem, ParentValue, Opts) when is_list(ParentValue) ->
    Search = lists:search(fun(Val) ->
        case ?get(AttrName, Val, '__undefined__') of
            '__undefined__' -> false;
            V -> V =:= AttrValue
        end
    end, ParentValue),
    case Search of
        false ->
            {ok, ParentElem, ParentValue};
        {value, ElemValue} ->
            ParentValueDel = ParentValue -- [ElemValue],
            case delete2(Rest, {ParentElem, Attr}, ElemValue, Opts) of
                {ok, _, []} ->
                    {ok, ParentElem, ParentValueDel};
                {ok, _, NewElemValue} ->
                    NewParentValue = [NewElemValue] ++ ParentValueDel,
                    {ok, ParentElem, NewParentValue};
                {error, Reason} ->
                    {error, Reason}
            end
    end;
delete2([{attr, _Attr} | _Rest], ParentElem, _ParentValue, _Opts) ->
    {error, {not_a_list, ParentElem}}.

do_delete(ParentValue, Elem, Opts) ->
    do_delete(ParentValue, Elem, Opts, []).

do_delete([], _Elem, _Opts, Acc) ->
    lists:reverse(Acc);
do_delete([Elem | Rest], Elem, Opts, Acc) ->
    case ?get(property, Opts, both) of
        both -> do_delete(Rest, Elem, Opts, Acc);
        atom -> do_delete(Rest, Elem, Opts, Acc);
        tuple -> do_delete(Rest, Elem, Opts, [Elem | Acc])
    end;
do_delete([{Elem, Value} | Rest], Elem, Opts, Acc) ->
    case ?get(property, Opts, both) of
        atom -> do_delete(Rest, Elem, Opts, [{Elem, Value} | Acc]);
        both -> do_delete(Rest, Elem, Opts, Acc);
        tuple -> do_delete(Rest, Elem, Opts, Acc)
    end;
do_delete([ElemOther | Rest], Elem, Opts, Acc) ->
    do_delete(Rest, Elem, Opts, [ElemOther |Acc]).

-spec delete(binary() | string(), emfa(), emfa(), properties()) -> {ok, any()} | {error, any()}.
delete(Path, MFA, {M,F,A}, Opts) ->
    case delete(Path, {mfa, MFA}, Opts) of
        {ok, RootElem, RootElemValue} ->
            case catch apply(M, F, [RootElem,RootElemValue]++A)  of
                {'EXIT', Reason} ->
                    {error, Reason};
                Result ->
                    {ok, Result}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
