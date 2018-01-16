%%%----------------------------------------------------------------------
%%% File    : ejabberd_hooks.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage hooks
%%% Created :  8 Aug 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_hooks).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% External exports
-export([start_link/0,
         add/4,
         add/5,
         delete/4,
         delete/5,
         run/2,
         run/3,
         run_fold/3,
         run_fold/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         handle_info/2,
         terminate/2]).

-export([add/1,
         delete/1]).

-include("ejabberd.hrl").

-type hook() :: {atom(), ejabberd:server() | global, module(), fun() | atom(), integer()}.

-record(state, {
          checked_modules=[],
          hooks_without_callback=[]
         }).

-export_type([hook/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ejabberd_hooks}, ejabberd_hooks, [], []).

%% @doc Add a fun to the given hook, if third argument is a function.
%% Add hook handler with the same function name, if third argument is a module.
%% The integer sequence is used to sort the calls:
%% low numbers are executed before high numbers.
-spec add(Hook :: atom(),
          Host :: ejabberd:server() | global,
          FunctionOrModule :: fun() | atom(),
          Seq :: integer()) -> ok.
add(Hook, Host, Function, Seq) when is_function(Function) ->
    add(Hook, Host, undefined, Function, Seq);
add(Hook, Host, Module, Seq) when is_atom(Module) ->
    add(Hook, Host, Module, Hook, Seq).

%% @doc Add a module and function to the given hook.
%% The integer sequence is used to sort the calls:
%% low numbers are executed before high numbers.
-spec add(Hook :: atom(),
          Host :: ejabberd:server() | global,
          Module :: atom(),
          Function :: fun() | atom(),
          Seq :: integer()) -> ok.
add(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, Host, Module, Function, Seq}).

-spec add([hook()]) -> ok.
add(Hooks) when is_list(Hooks) ->
    add_or_del(add, Hooks).

-spec add_or_del(add | delete, [hook()]) -> ok.
add_or_del(AddOrDel, Hooks) ->
    [erlang:apply(?MODULE, AddOrDel, tuple_to_list(Hook)) ||
     Hook <- Hooks],
    ok.

%% @doc Delete a module and function from this hook, if third argument is a function.
%% Deletes hook handler, where hook name and function name are equal,
%% if third argument is a module.
%% It is important to indicate exactly the same information than when the call was added.
-spec delete(Hook :: atom(),
             Host :: ejabberd:server() | global,
             FunctionOrModule :: fun() | atom(),
             Seq :: integer()) -> ok.
delete(Hook, Host, Function, Seq) when is_function(Function) ->
    delete(Hook, Host, undefined, Function, Seq);
delete(Hook, Host, Module, Seq) when is_atom(Module) ->
    delete(Hook, Host, Module, Hook, Seq).

-spec delete(Hook :: atom(),
             Host :: ejabberd:server() | global,
             Module :: atom(),
             Function :: fun() | atom(),
             Seq :: integer()) -> ok.
delete(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {delete, Hook, Host, Module, Function, Seq}).

-spec delete([hook()]) -> ok.
delete(Hooks) when is_list(Hooks) ->
    add_or_del(delete, Hooks).

%% @doc Run the calls of this hook in order, don't care about function results.
%% If a call returns stop, no more calls are performed.
-spec run(Hook :: atom(),
          Args :: [any()]) -> ok.
run(Hook, Args) ->
    run_fold(Hook, global, #{}, Args).

-spec run(Hook :: atom(),
          Host :: ejabberd:server() | global,
          Args :: [any()]) -> ok.
run(Hook, Host, Args) ->
    run_fold(Hook, Host, mongoose_acc:new(), Args).

%% @spec (Hook::atom(), Val, Args) -> Val | stopped | NewVal
%% @doc Run the calls of this hook in order.
%% The arguments passed to the function are: [Val | Args].
%% The result of a call is used as Val for the next call.
%% If a call returns 'stop', no more calls are performed and 'stopped' is returned.
%% If a call returns {stop, NewVal}, no more calls are performed and NewVal is returned.
run_fold(Hook, Val, Args) ->
    run_fold(Hook, global, Val, Args).

run_fold(Hook, Host, Val, Args) ->
    Res = case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            mongoose_metrics:increment_generic_hook_metric(Host, Hook),
            run_fold1(Ls, Hook, Val, Args);
        [] ->
            Val
    end,
    record(Hook, Res).

record(_Hook, Acc) ->
    Acc.
    % just to show some nice things we can do now
    % this should probably be protected by a compilation flag
    % unless load tests show that the impact on performance is negligible
%%    case mongoose_acc:is_acc(Acc) of % this check will go away some day
%%        true ->
%%            mongoose_acc:append(hooks_run, Hook, Acc);
%%        false ->
%%            Acc
%%    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    ets:new(hooks, [named_table, {read_concurrency, true}]),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({add, Hook, Host, Module, Function, Seq}, _From, State) ->
    check_function_name(Hook, Host, Module, Function, Seq),
    State2 = check_module(Module, State),
    State3 = check_hook_callback(Hook, Host, Module, Function, Seq, State2),
    Reply = case ets:lookup(hooks, {Hook, Host}) of
                [{_, Ls}] ->
                    El = {Seq, Module, Function},
                    case lists:member(El, Ls) of
                        true ->
                            ok;
                        false ->
                            NewLs = lists:merge(Ls, [El]),
                            ets:insert(hooks, {{Hook, Host}, NewLs}),
                            ok
                    end;
                [] ->
                    NewLs = [{Seq, Module, Function}],
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
                    mongoose_metrics:create_generic_hook_metric(Host, Hook),
                    ok
            end,
    {reply, Reply, State3};

handle_call({delete, Hook, Host, Module, Function, Seq}, _From, State) ->
    Reply = case ets:lookup(hooks, {Hook, Host}) of
                [{_, Ls}] ->
                    NewLs = lists:delete({Seq, Module, Function}, Ls),
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
                    ok;
                [] ->
                    ok
            end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:delete(hooks),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

run_fold1([], _Hook, Val, _Args) ->
    Val;
run_fold1([{_Seq, Module, Function} | Ls], Hook, Val, Args) ->
    Res = hook_apply_function(Module, Function, Hook, Val, Args),
    case Res of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nrunning hook: ~p",
                       [Reason, {Hook, Args}]),
            run_fold1(Ls, Hook, Val, Args);
        stop ->
            stopped;
        {stop, NewVal} ->
            NewVal;
        NewVal ->
            run_fold1(Ls, Hook, NewVal, Args)
    end.

hook_apply_function(_Module, Function, _Hook, Val, Args) when is_function(Function) ->
    safely:apply(Function, [Val | Args]);
hook_apply_function(Module, Function, Hook, Val, Args) ->
    Result = safely:apply(Module, Function, [Val | Args]),
    record(Hook, Module, Function, Result).


record(_Hook, _Module, _Function, Acc) ->
    Acc.
    % just to show some nice things we can do now
    % this should probably be protected by a compilation flag
    % unless load tests show that the impact on performance is negligible
%%    case mongoose_acc:is_acc(Acc) of % this check will go away some day
%%        true ->
%%            mongoose_acc:append(handlers_run, {Hook, Module, Function}, Acc);
%%        false ->
%%            Acc
%%    end.


%% Check that function name is the same as hook name
check_function_name(Hook, _Host, Module, Function, _Seq)
  when is_atom(Function), Hook =/= Function ->
    %% Hook and Function should be the same
    ?WARNING_MSG("issue=hook_format_depricated "
                 "module=~p "
                 "hook=~p "
                 "function=~p ",
                 [Module, Hook, Function]);
check_function_name(_Hook, _Host, _Module, _Function, _Seq) ->
    ok.

%% Check that the caller module has hook_handler behaviour
check_module(Module, State=#state{checked_modules=Modules}) ->
    case lists:member(Module, Modules) of
        true ->
            State;
        false ->
            check_module_behaviour(Module),
            check_function_name_conflicts(Module),
            State#state{checked_modules=[Module|Modules]}
    end.

check_module_behaviour(Module) ->
    try lists:member({behavior,[hook_handler]}, Module:module_info(attributes)) of
        true ->
            ok;
        false ->
            ?WARNING_MSG("issue=hook_handler_behaviour_missing "
                         "module=~p ",
                         [Module])
        catch Error:Reason ->
            ?ERROR_MSG("issue=check_module_behaviour_failed "
                         "module=~p reason=~p:~p",
                         [Module, Error, Reason])
    end,
    ok.

check_function_name_conflicts(Module) ->
    try
        Exports = Module:module_info(exports),
        AllCallbacks = hook_handler:behaviour_info(callbacks),
        lists:foreach(fun({Function, Arity}) ->
                checked_function_name_conflict(Module, Function, Arity, AllCallbacks)
              end, Exports)
        catch Error:Reason ->
            ?ERROR_MSG("issue=checked_function_name_conflicts_failed "
                         "module=~p reason=~p:~p",
                         [Module, Error, Reason])
    end,
    ok.

checked_function_name_conflict(Module, Function, Arity, AllCallbacks) ->
    Conflict = lists:keymember(Function, 1, AllCallbacks),
    Allowed = lists:member({Function, Arity}, AllCallbacks),
    case {Conflict, Allowed} of
        {true, false} ->
            %% The function should be renamed, if it's not a hook handler.
            %% If it's a hook handler - most likely it's broken.
            ?WARNING_MSG("issue=confusing_function_name function=~p:~p/~p",
                         [Module, Function, Arity]);
        _ ->
            ok
    end.

%% Check that hook has callback defined inside hook_handler module
check_hook_callback(Hook, Host, Module, Function, Seq,
                    State=#state{hooks_without_callback=Hooks}) ->
    AllCallbacks = hook_handler:behaviour_info(callbacks),
    MatchedCallbacks = [{H, A} || {H, A} <- AllCallbacks, H =:= Hook],
    case MatchedCallbacks of
        [] ->
            case lists:member(Hook, Hooks) of
                true ->
                    State; %% already reported
                false ->
                    %% Add missing callback definition into hook_handlers.
                    %% Use Module as a reference point.
                    ?WARNING_MSG("issue=missing_hook_callback hook=~p module=~p",
                                 [Hook, Module]),
                    State#state{hooks_without_callback=[Hook|Hooks]}
            end;
        [{Hook, Arity}] ->
            check_hanlder_arity(Arity, Hook, Host, Module, Function, Seq),
            State;
        _ ->
            ?ERROR_MSG("issue=more_than_one_callback matched=~p",
                       [MatchedCallbacks])
    end.

%% Still check arity even if Hook =/= Function
check_hanlder_arity(Arity, Hook, _Host, Module, Function, Seq) ->
    try
        Exports = Module:module_info(exports),
        case lists:member({Function, Arity}, Exports) of
            true ->
                ok;
            false ->
                ?ERROR_MSG("issue=wrong_arity module=~p function=~p hook=~p expected=~p",
                           [Module, Function, Hook, Arity]),
                ok
        end
        catch Error:Reason ->
            ?ERROR_MSG("issue=check_hanlder_arity_failed module=~p reason=~p:~p",
                       [Module, Error, Reason]),
            ok
    end.
