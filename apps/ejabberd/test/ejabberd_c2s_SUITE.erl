-module(ejabberd_c2s_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/src/ejabberd_c2s.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(ne(E, I), ?assert(E =/= I)).


all() -> [ c2s_start_stop_test,
           c2s_inits_to_wait_for_stream,
           c2s_goes_to_feature_request_after_stream_start
         ].

init_per_suite(C) ->
    stringprep:start(),
    xml:start(),
    C.

init_per_testcase(_, C) ->
    {ok, Pid} = create_c2s(),
    [{c2s_pid, Pid} | C].

end_per_testcase(_, C) ->
    {c2s_pid, Pid} = lists:keyfind(c2s_pid, 1, C),
    stop_c2s(Pid), timer:sleep(200).

c2s_start_stop_test(C) ->
    C2SPid = given_c2s_started(C),
    when_c2s_is_stopped(C2SPid),
    %% then
    ?eq(false, erlang:is_process_alive(C2SPid)).

c2s_inits_to_wait_for_stream(C) ->
    C2SPid = when_c2s_started(C),
    %% then
    ?eq(wait_for_stream, c2s_state(C2SPid, state_name)).

c2s_goes_to_feature_request_after_stream_start(C) ->
    C2SPid = given_c2s_started(C),
    % when
    c2s_gets_stream_start(C2SPid),
    timer:sleep(1000),
%    true = is_process_alive(C2SPid),
    ct:pal("~p", [process_info(C2SPid)]),
    %% then
    ?eq(wait_for_feature_request, c2s_state(C2SPid, state_name)).


given_c2s_started(C) -> element(2,lists:keyfind(c2s_pid, 1, C)).
when_c2s_started(C) -> given_c2s_started(C).
when_c2s_is_stopped(Pid) -> stop_c2s(Pid).

c2s_gets_stream_start(C2SPid) ->
    send_el(C2SPid, stream_start()).

send_el(C2SPid, #xmlel{} = Element) ->
    gen_fsm:send_event(C2SPid, {xmlstreamelement, Element});
send_el(C2SPid, Element) ->
    gen_fsm:send_event(C2SPid, Element).

create_c2s() ->
    ejabberd_c2s_SUITE_mocks:setup(),
    ejabberd_c2s:start_link({ejabberd_socket, self()}, c2s_default_opts()).

c2s_default_opts() ->
    [{access, c2s},
     {shaper, c2s_shaper},
     {max_stanza_size, 65536}].

stop_c2s(C2SPid) when is_pid(C2SPid) ->
    _R = ejabberd_c2s:stop(C2SPid),
    ejabberd_c2s_SUITE_mocks:teardown().

jid(Str) ->
    jlib:binary_to_jid(Str).

c2s_state(C2SPid, state_name) ->
    {status,C2SPid, _, [_,running,_,[]|[Data|_]]} = sys:get_status(C2SPid),
    [_Header,{data, Status},{data,_StateData}] = Data,
    element(2, lists:keyfind("StateName",1,Status)).


%% xml elements
stream_start() ->
    #xmlstreamstart{name= <<"stream:stream">>,
                    attrs=[{<<"to">>, <<"localhost">>},
                           {<<"version">>, <<"1.0">>},
                           {<<"xmlns">>, <<"jabberd:client">>}]}.



%%
%% @TODO: Implement these previously internal eunit tests as part of this
%% test suite
%%

%-include_lib("eunit/include/eunit.hrl").
%-compile([export_all]).
%-define(_eq(E, I), ?_assertEqual(E, I)).
%-define(eq(E, I), ?assertEqual(E, I)).
%-define(ne(E, I), ?assert(E =/= I)).
%
%%%
%%% Tests
%%%
%
%increment_sm_incoming_test_() ->
%    [?LET(I, fun increment_sm_incoming/1,
%      [?_eq(1,        I(0)),
%       ?_eq(2,        I(1)),
%       ?_eq(3,        I(2)),
%       ?_eq(10000000, I(9999999)),
%       ?_eq(0,        I(?STREAM_MGMT_H_MAX))]),
%     ?LET(I, fun increment_sm_counter/2,
%      [?_eq(4,        I(?STREAM_MGMT_H_MAX, 5))])].
%
%calc_to_drop_test_() ->
%    C = fun calc_to_drop/2,
%    [?_eq(0, C(0, 0)),
%     ?_eq(1, C(2, 1)),
%     ?_eq(2, C(5, 3)),
%     ?_eq(4, C(2, ?STREAM_MGMT_H_MAX - 1))].
%
%drop_last_test_() ->
%    D = fun drop_last/2,
%    [?_eq({0, []},        D(0, [])),
%     ?_eq({1, []},        D(1, [1])),
%     ?_eq({2, []},        D(2, [1, 2])),
%     ?_eq({0, [1, 2]},    D(0, [1, 2])),
%     ?_eq({0, []},        D(2, [])),
%     ?_eq({1, [1]},       D(1, [1, 2])),
%     ?_eq({3, [1, 2, 3]}, D(3, [1, 2, 3, 4, 5, 6])),
%     ?_eq({6, []},        D(7, [1, 2, 3, 4, 5, 6]))].
%
%buffer_outgoing_test_() ->
%    {setup, fun create_c2s/0, fun cleanup_c2s/1,
%     {with, [fun starts_with_empty_buffer/1,
%         fun buffer_outgoing/1]}}.
%
%client_ack_test_() ->
%    {setup, fun create_c2s/0, fun cleanup_c2s/1,
%     {with, [fun starts_with_empty_buffer/1,
%         fun buffer_outgoing/1,
%         fun buffer_outgoing/1,
%         fun buffer_outgoing/1,
%         fun buffer_outgoing/1,
%         mk_assert_acked(0),
%         mk_client_ack(3)]}}.
%
%no_buffer_test_() ->
%    {setup, fun () -> c2s_initial_state(mgmt_on) end,
%     {with, [fun (State0) ->
%             State = State0#state{stream_mgmt_buffer_max = infinity},
%             ?eq([], State#state.stream_mgmt_buffer),
%             NewState = buffer_out_stanza(fake_packet, State),
%             ?eq([fake_packet], NewState#state.stream_mgmt_buffer)
%         end,
%         fun (State0) ->
%             State = State0#state{stream_mgmt_buffer_max = no_buffer},
%             ?eq([], State#state.stream_mgmt_buffer),
%             NewState = buffer_out_stanza(fake_packet, State),
%             ?eq([], NewState#state.stream_mgmt_buffer)
%         end]}}.
%
%enable_stream_resumption_test_() ->
%    {setup,
%     %% Mecked fun must send a message to the test runner to synchronize;
%     %% to write that fun we must know this process's pid beforehand;
%     %% we know self() beforehand, so run the test in the current process.
%     local,
%     fun () ->
%             Self = self(),
%             meck:new(mod_stream_management, []),
%             meck:expect(mod_stream_management, get_buffer_max,
%                         fun (_) -> 100 end),
%             meck:expect(mod_stream_management, get_ack_freq,
%                         fun (_) -> 1 end),
%             meck:expect(mod_stream_management, get_resume_timeout,
%                         fun (DefaultValue) -> DefaultValue end),
%             meck:expect(mod_stream_management, register_smid,
%                         fun (_SMID, _SID) ->
%                                 Self ! register_smid_called,
%                                 ok
%                         end),
%             create_c2s(c2s_initial_state())
%     end,
%     fun (C2S) ->
%             cleanup_c2s(C2S),
%             meck:unload(mod_stream_management)
%     end,
%     {with, [fun (C2S) ->
%                     Enable = #xmlel{name = <<"enable">>,
%                                     attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
%                                              {<<"resume">>, <<"true">>}]},
%                     ?GEN_FSM:send_event(C2S, {xmlstreamelement, Enable}),
%                     receive
%                         register_smid_called ->
%                             ?assert(meck:called(mod_stream_management,
%                                                 register_smid, 2)),
%                             S = status_to_state(sys:get_status(C2S)),
%                             ?ne(undefined, S#state.stream_mgmt_id)
%                     after 1000 ->
%                               error("SMID not registered")
%                     end
%             end]}}.
%
%%%
%%% Helpers
%%%
%
%create_c2s() ->
%    create_c2s(c2s_initial_state(mgmt_on)).
%
%create_c2s(State) ->
%    meck:new(ejabberd_sm),
%    meck:expect(ejabberd_sm, close_session, fun(_SID, _User, _Server, _Resource) -> ok end),
%    meck:new(ejabberd_socket),
%    meck:expect(ejabberd_socket, close,
%        fun(_) ->
%            ?ERROR_MSG("socket closed too early!~n", [])
%        end),
%    meck:expect(ejabberd_socket, send, fun(_,_) -> ok end),
%    meck:new(ejabberd_hooks),
%    meck:expect(ejabberd_hooks, run, fun(_,_) -> ok end),
%    meck:expect(ejabberd_hooks, run, fun(_,_,_) -> ok end),
%    meck:expect(ejabberd_hooks, run_fold,
%        fun(privacy_check_packet, _, _, _) -> allow end),
%    F = fun() ->
%        ?GEN_FSM:enter_loop(?MODULE, [], session_established, State)
%    end,
%    proc_lib:spawn_link(F).
%
%c2s_initial_state(mgmt_on) ->
%    S = c2s_initial_state(),
%    S#state{stream_mgmt = true}.
%
%c2s_initial_state() ->
%    Jid = jid(<<"qwe@localhost/eunit">>),
%    {U, S, R} = {<<"qwe">>, <<"localhost">>, <<"eunit">>},
%    #state{jid = Jid,
%       user = U, server = S, resource = R,
%       sockmod = ejabberd_socket}.
%
%cleanup_c2s(C2S) when is_pid(C2S) ->
%    exit(C2S, normal),
%    meck:unload(ejabberd_hooks),
%    meck:unload(ejabberd_socket),
%    meck:unload(ejabberd_sm).
%
%starts_with_empty_buffer(C2S) ->
%    S = status_to_state(sys:get_status(C2S)),
%    ?eq(0, length(S#state.stream_mgmt_buffer)).
%
%buffer_outgoing(C2S) ->
%    S1 = status_to_state(sys:get_status(C2S)),
%    BufferSize = length(S1#state.stream_mgmt_buffer),
%    C2S ! {route, jid(<<"asd@localhost">>), jid(<<"qwe@localhost">>), message(<<"hi">>)},
%    S2 = status_to_state(sys:get_status(C2S)),
%    ?eq(BufferSize+1, length(S2#state.stream_mgmt_buffer)).
%
%status_to_state({status, _Pid, {module, ?GEN_FSM}, Data}) ->
%    [_, _, _, _, [{_, _}, {_, _}, {_, [{"StateData", State}]}]] = Data,
%    State.
%
%jid(Str) ->
%    jlib:binary_to_jid(Str).
%
%message(Content) ->
%    Body = #xmlel{name = <<"body">>,
%                  children = [#xmlcdata{content = Content}]},
%    #xmlel{name = <<"message">>,
%           attrs = [{<<"type">>, "chat"}],
%           children = [Body]}.
%
%mk_assert_acked(X) ->
%    fun(C2S) ->
%        S = status_to_state(sys:get_status(C2S)),
%        ?eq(X, S#state.stream_mgmt_out_acked)
%    end.
%
%mk_client_ack(H) ->
%    fun(C2S) ->
%        S1 = status_to_state(sys:get_status(C2S)),
%        Acked = S1#state.stream_mgmt_out_acked,
%        BufferSize = length(S1#state.stream_mgmt_buffer),
%        H = 3,
%        ?eq(BufferSize, length(S1#state.stream_mgmt_buffer)),
%        C2S ! {'$gen_event', {xmlstreamelement, ack(H)}},
%        S2 = status_to_state(sys:get_status(C2S)),
%        ?eq(H, S2#state.stream_mgmt_out_acked),
%        ?eq(BufferSize - (H - Acked), length(S2#state.stream_mgmt_buffer))
%    end.
%
%ack(H) ->
%    #xmlel{name = <<"a">>,
%           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
%                    {<<"h">>, integer_to_binary(H)}]}.
%
