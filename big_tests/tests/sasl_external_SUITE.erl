-module(sasl_external_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

all() ->
    [
%%     {group, fast_tls},
     {group, just_tls},
     {group, just_tls_use_common_name},
     {group, just_tls_allow_just_user_identity},
%%     {group, fast_tls_allow_self_signed},
     {group, just_tls_allow_self_signed}].

groups() ->
    [ {encode_group_name(TLSMod, BaseGroup, Signed), Opts, Cases}
      || {BaseGroup, Opts, Cases} <- base_groups(),
          TLSMod <- [<<"just_tls">>, <<"fast_tls">>],
          Signed <- [<<"ca_signed">>, <<"self_signed">>] ].

%% groups() ->
%%     G = [{fast_tls, [parallel], common_test_cases() ++ no_allowed_self_signed_test_cases()},
%%          {just_tls, [parallel], common_test_cases() ++ no_allowed_self_signed_test_cases()},
%%          {fast_tls_use_common_name, use_common_name_test_cases()},
%%          {just_tls_use_common_name, use_common_name_test_cases()},
%%          {fast_tls_allow_just_user_identity, allow_just_user_identity_test_cases()},
%%          {just_tls_allow_just_user_identity, allow_just_user_identity_test_cases()},
%% %%         {fast_tls_allow_self_signed, [parallel], common_test_cases() ++ self_signed_test_cases()},
%%          {just_tls_allow_self_signed, [parallel], common_test_cases() ++ self_signed_test_cases()},
%%          {self_signed_use_common_name, [parallel], self_signed_use_common_name_test_cases()}],
%%     ct_helper:repeat_all_until_all_ok(G).

base_groups() ->
    [
     {standard, [parallel], common_test_cases(<<"ca_signed">>)}
    ].

encode_group_name(TLSMod, BaseName, Signed) ->
    binary_to_atom(<<TLSMod/binary, $+, (atom_to_binary(BaseName, utf8))/binary, $+, Signed/binary>>, utf8).

decode_group_name(ComplexName) ->
    [TLSMod, BaseName, Signed] = binary:split(atom_to_binary(ComplexName, utf8), <<"+">>, [global]),
    #{tls_module => TLSMod, base_name => binary_to_atom(BaseName, utf8), signed => Signed}.

common_test_cases(_Signed) ->
    [
     cert_no_xmpp_addrs_fails_ca_signed,
     cert_no_xmpp_addrs_no_identity_ca_signed,

     cert_one_xmpp_addr_identity_correct_ca_signed,
     cert_one_xmpp_addrs_no_identity_ca_signed,
%%     cert_one_xmpp_addr_wrong_hostname,

     cert_more_xmpp_addrs_identity_correct_ca_signed,
     cert_more_xmpp_addrs_no_identity_fails_ca_signed,
%%     cert_more_xmpp_addrs_wrong_identity_fails,

     cert_xmpp_addrs_request_name_empty_ws,
     cert_xmpp_addrs_request_name_empty_bosh,
     no_cert_fails_to_authenticate
    ].

use_common_name_test_cases() ->
    [
    cert_with_cn_no_xmpp_addrs_identity_correct_ca_signed,
    cert_with_cn_no_xmpp_addrs_wrong_identity_fails_ca_signed,
    cert_with_cn_no_xmpp_addrs_no_identity_ca_signed
    ].
allow_just_user_identity_test_cases() ->
    [
     cert_no_xmpp_addrs_just_use_identity_ca_signed
    ].

self_signed_test_cases() ->
    [self_signed_cert_is_allowed_with_tls,
     cert_more_xmpp_addrs_identity_correct_self_signed,
     cert_no_xmpp_addrs_fails_self_signed,
     cert_no_xmpp_addrs_no_identity_self_signed,
     cert_one_xmpp_addrs_no_identity_self_signed,
     cert_more_xmpp_addrs_no_identity_fails_self_signed,
     cert_one_xmpp_addr_identity_correct_self_signed,
     self_signed_cert_is_allowed_with_ws,
     self_signed_cert_is_allowed_with_bosh].

self_signed_use_common_name_test_cases() ->
    [
     cert_with_cn_no_xmpp_addrs_identity_correct_self_signed,
     cert_with_cn_no_xmpp_addrs_wrong_identity_fails_self_signed,
     cert_with_cn_no_xmpp_addrs_no_identity_self_signed
    ].

%% self_signed_allow_just_user_identity_test_cases() ->
%%     [
%%      cert_no_xmpp_addrs_just_use_identity_self_signed
%%     ].



no_allowed_self_signed_test_cases() ->
    [self_signed_cert_fails_to_authenticate_with_tls,
     self_signed_cert_fails_to_authenticate_with_ws,
     self_signed_cert_fails_to_authenticate_with_bosh].

init_per_suite(Config) ->
    Config0 = escalus:init_per_suite(Config),
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    generate_certs(Config1).

end_per_suite(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(mongooseim),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    CACertFile = filename:join([path_helper:repo_dir(Config),
				"tools", "ssl", "ca-clients", "cacert.pem"]),
    TLSModule = tls_module_by_group_name(GroupName),
    NewConfigValues = [{tls_config, "{certfile, \"priv/ssl/fake_server.pem\"},"
			            "starttls, verify_peer,"
				    "{cafile, \"" ++ CACertFile ++ "\"},"
                                    ++ ssl_options_by_group_name(GroupName)},
		       {tls_module, "{tls_module, " ++ atom_to_list(TLSModule) ++ "},"},
		       {https_config,  "{ssl, [{certfile, \"priv/ssl/fake_cert.pem\"},"
			                      "{keyfile, \"priv/ssl/fake_key.pem\"}, {password, \"\"},"
				              "{verify, verify_peer}," ++ verify_mode_by_group_name(GroupName) ++
					      "{cacertfile, \"" ++ CACertFile ++ "\"}]},"},
               {cyrsasl_external, "{cyrsasl_external," ++ config_by_group_name(GroupName) ++ "}"},
		       {auth_method, "pki"},
		       {sasl_mechanisms, "{sasl_mechanisms, [cyrsasl_external]}."}],
    ejabberd_node_utils:modify_config_file(NewConfigValues, Config),
    ejabberd_node_utils:restart_application(mongooseim),
    Config.

config_by_group_name(just_tls_use_common_name) ->
   "use_common_name";
config_by_group_name(fast_tls_use_common_name) ->
   "use_common_name";
config_by_group_name(just_tls_allow_just_user_identity) ->
    "allow_just_user_identity";
config_by_group_name(fast_tls_allow_just_user_identity) ->
    "allow_just_user_identity";
config_by_group_name(_GroupName) ->
    "standard".

tls_module_by_group_name(fast_tls) ->
    fast_tls;
tls_module_by_group_name(just_tls) ->
    just_tls;
tls_module_by_group_name(fast_tls_use_common_name) ->
    fast_tls;
tls_module_by_group_name(just_tls_use_common_name) ->
    just_tls;
tls_module_by_group_name(fast_tls_allow_just_user_identity) ->
    fast_tls;
tls_module_by_group_name(just_tls_allow_just_user_identity) ->
    just_tls;
tls_module_by_group_name(just_tls_allow_self_signed) ->
    just_tls;
tls_module_by_group_name(fast_tls_allow_self_signed) ->
    fast_tls.

ssl_options_by_group_name(fast_tls) ->
    "";
ssl_options_by_group_name(just_tls) ->
    "{ssl_options, [{verify_fun, {peer, false}}]},";
ssl_options_by_group_name(fast_tls_use_common_name) ->
    "";
ssl_options_by_group_name(just_tls_use_common_name) ->
    "{ssl_options, [{verify_fun, {peer, false}}]},";
ssl_options_by_group_name(fast_tls_allow_just_user_identity) ->
    "";
ssl_options_by_group_name(just_tls_allow_just_user_identity) ->
    "{ssl_options, [{verify_fun, {peer, false}}]},";
ssl_options_by_group_name(just_tls_allow_self_signed) ->
    "{ssl_options, [{verify_fun, {selfsigned_peer, true}}]},";
ssl_options_by_group_name(fast_tls_allow_self_signed) ->
    "{ssl_options, [{verify_fun, {selfsigned_peer, true}}]},".

verify_mode_by_group_name(fast_tls) ->
    "";
verify_mode_by_group_name(just_tls) ->
    "";
verify_mode_by_group_name(fast_tls_use_common_name) ->
    "";
verify_mode_by_group_name(just_tls_use_common_name) ->
    "";
verify_mode_by_group_name(fast_tls_allow_just_user_identity) ->
    "";
verify_mode_by_group_name(just_tls_allow_just_user_identity) ->
    "";
verify_mode_by_group_name(just_tls_allow_self_signed) ->
    "{verify_mode, selfsigned_peer},";
verify_mode_by_group_name(fast_tls_allow_self_signed) ->
    "{verify_mode, selfsigned_peer},".


end_per_group(_, Config) ->
    Config.

cert_more_xmpp_addrs_identity_correct_ca_signed(C) ->
    cert_more_xmpp_addrs_identity_correct(C, "not-alice").
cert_more_xmpp_addrs_identity_correct_self_signed(C) ->
    cert_more_xmpp_addrs_identity_correct(C, "not-alice-self-signed").
cert_more_xmpp_addrs_identity_correct(C, User) ->
    %% More than one xmpp_addr and specified identity, common_name not used
    UserSpec = [{requested_name, <<"alice@localhost">>} |
		generate_user_tcp(C, User)],
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_one_xmpp_addr_identity_correct_ca_signed(C) ->
    cert_one_xmpp_addr_identity_correct(C, "bob").
cert_one_xmpp_addr_identity_correct_self_signed(C) ->
    cert_one_xmpp_addr_identity_correct(C, "bob-self-signed").
cert_one_xmpp_addr_identity_correct(C, User) ->
    UserSpec = [{requested_name, <<"bob@localhost">>} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

cert_no_xmpp_addrs_fails_ca_signed(C) ->
    cert_no_xmpp_addrs_fails(C, "john").
cert_no_xmpp_addrs_fails_self_signed(C) ->
    cert_no_xmpp_addrs_fails(C, "john-self-signed").
cert_no_xmpp_addrs_fails(C, User) ->
    UserSpec = [{requested_name, <<"john@localhost">>} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

cert_no_xmpp_addrs_just_use_identity_ca_signed(C) ->
    cert_no_xmpp_addrs_just_use_identity(C, "not-mike").
cert_no_xmpp_addrs_just_use_identity_self_signed(C) ->
    cert_no_xmpp_addrs_just_use_identity(C, "not-mike-self-signed").
cert_no_xmpp_addrs_just_use_identity(C, User) ->
    UserSpec = [{requested_name, <<"mike@localhost">>} |
		generate_user_tcp(C, User)],
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_no_xmpp_addrs_no_identity_ca_signed(C) ->
    cert_no_xmpp_addrs_no_identity(C, "john").
cert_no_xmpp_addrs_no_identity_self_signed(C) ->
    cert_no_xmpp_addrs_no_identity(C, "john-self-signed").
cert_no_xmpp_addrs_no_identity(C, User) ->
    UserSpec = generate_user_tcp(C, User),
    cert_fails_to_authenticate(UserSpec).

cert_more_xmpp_addrs_no_identity_fails_ca_signed(C) ->
    cert_more_xmpp_addrs_no_identity_fails(C, "not-alice").
cert_more_xmpp_addrs_no_identity_fails_self_signed(C) ->
    cert_more_xmpp_addrs_no_identity_fails(C, "not-alice-self-signed").
cert_more_xmpp_addrs_no_identity_fails(C, User) ->
    UserSpec = generate_user_tcp(C, User),
    cert_fails_to_authenticate(UserSpec).

cert_one_xmpp_addrs_no_identity_ca_signed(C) ->
    cert_one_xmpp_addrs_no_identity(C, "bob").
cert_one_xmpp_addrs_no_identity_self_signed(C) ->
    cert_one_xmpp_addrs_no_identity(C, "bob-self-signed").
cert_one_xmpp_addrs_no_identity(C, User) ->
    UserSpec = generate_user_tcp(C, User),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_with_cn_no_xmpp_addrs_no_identity_ca_signed(C) ->
    cert_with_cn_no_xmpp_addrs_no_identity(C, "john").
cert_with_cn_no_xmpp_addrs_no_identity_self_signed(C) ->
    cert_with_cn_no_xmpp_addrs_no_identity(C, "john-self-signed").
cert_with_cn_no_xmpp_addrs_no_identity(C, User) ->
    UserSpec = generate_user_tcp(C, User),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_with_cn_no_xmpp_addrs_identity_correct_ca_signed(C) ->
    cert_with_cn_no_xmpp_addrs_identity_correct(C, "john").
cert_with_cn_no_xmpp_addrs_identity_correct_self_signed(C) ->
    cert_with_cn_no_xmpp_addrs_identity_correct(C, "john-self-signed").
cert_with_cn_no_xmpp_addrs_identity_correct(C, User) ->
    UserSpec = [{requested_name, <<"john@localhost">>} |
                generate_user_tcp(C, User)],
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

cert_with_cn_no_xmpp_addrs_wrong_identity_fails_ca_signed(C) ->
    cert_with_cn_no_xmpp_addrs_wrong_identity_fails(C, "not-mike").
cert_with_cn_no_xmpp_addrs_wrong_identity_fails_self_signed(C) ->
    cert_with_cn_no_xmpp_addrs_wrong_identity_fails(C, "not-mike-self-signed").
cert_with_cn_no_xmpp_addrs_wrong_identity_fails(C, User) ->
    UserSpec = [{requested_name, <<"mike@localhost">>} |
                generate_user_tcp(C, User)],
    cert_fails_to_authenticate(UserSpec).

%% cert_more_xmpp_addrs_wrong_identity_fails_ca_signed(C) ->
%% cert_more_xmpp_addrs_wrong_identity_fails(C) ->
%% cert_more_xmpp_addrs_wrong_identity_fails_self_signed(C) ->
%% cert_more_xmpp_addrs_wrong_identity_fails(C) ->
%% cert_more_xmpp_addrs_wrong_identity_fails(C) ->
%%     UserSpec = [{requested_name, <<"grace@localhost">>} |
%% 		generate_user_tcp(C, "grace")],
%%     cert_fails_to_authenticate(UserSpec).
%% 
%% cert_one_xmpp_addr_wrong_hostname_ca_signed(C) ->
%% cert_one_xmpp_addr_wrong_hostname(C) ->
%% cert_one_xmpp_addr_wrong_hostname_self_signed(C) ->
%% cert_one_xmpp_addr_wrong_hostname(C) ->
%% cert_one_xmpp_addr_wrong_hostname(C) ->
%%     UserSpec = [{requested_name, <<"bob@fed1">>} |
%% 		generate_user_tcp(C, "bob")],
%%     cert_fails_to_authenticate(UserSpec).

cert_xmpp_addrs_request_name_empty_ws(C) ->
    UserSpec = generate_user(C, "bob", escalus_ws),
    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).

cert_xmpp_addrs_request_name_empty_bosh(C) ->
    UserSpec = generate_user(C, "bob", escalus_bosh),
    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).

self_signed_cert_fails_to_authenticate_with_tls(C) ->
    self_signed_cert_fails_to_authenticate(C, escalus_tcp).

self_signed_cert_fails_to_authenticate_with_ws(C) ->
    self_signed_cert_fails_to_authenticate(C, escalus_ws).

self_signed_cert_fails_to_authenticate_with_bosh(C) ->
    self_signed_cert_fails_to_authenticate(C, escalus_bosh).

cert_fails_to_authenticate(UserSpec) ->
    Self = self(),
    F = fun() ->
		{ok, Client, _} = escalus_connection:start(UserSpec),
		Self ! escalus_connected,
		escalus_connection:stop(Client)
	end,
    %% We spawn the process trying to connect because otherwise the testcase may crash
    %% due linked process crash (client's process are started with start_link)
    Pid = spawn(F),
    MRef = erlang:monitor(process, Pid),

    receive
	{'DOWN', MRef, process, Pid, _Reason} ->
	    ok;
	escalus_connected ->
	    ct:fail(authenticated_but_should_not)
    after 10000 ->
	      ct:fail(timeout_waiting_for_authentication_error)
    end.

self_signed_cert_fails_to_authenticate(C, EscalusTransport) ->
    Self = self(),
    F = fun() ->
		UserSpec = generate_user(C, "bob-self-signed", EscalusTransport),
		{ok, Client, _} = escalus_connection:start(UserSpec),
		Self ! escalus_connected,
		escalus_connection:stop(Client)
	end,
    %% We spawn the process trying to connect because otherwise the testcase may crash
    %% due linked process crash (client's process are started with start_link)
    Pid = spawn(F),
    MRef = erlang:monitor(process, Pid),

    receive
	{'DOWN', MRef, process, Pid, _Reason} ->
	    ok;
	escalus_connected ->
	    ct:fail(authenticated_but_should_not)
    after 10000 ->
	      ct:fail(timeout_waiting_for_authentication_error)
    end.

self_signed_cert_is_allowed_with_tls(C) ->
    self_signed_cert_is_allowed_with(escalus_tcp, C).

self_signed_cert_is_allowed_with_ws(C) ->
    self_signed_cert_is_allowed_with(escalus_ws, C).

self_signed_cert_is_allowed_with_bosh(C) ->
    self_signed_cert_is_allowed_with(escalus_bosh, C).

self_signed_cert_is_allowed_with(EscalusTransport, C) ->
    UserSpec = generate_user(C, "bob-self-signed", EscalusTransport),
    {ok, Client, _} = escalus_connection:start(UserSpec),
    escalus_connection:stop(Client).

no_cert_fails_to_authenticate(_C) ->
    UserSpec = [{username, <<"no_cert_user">>},
		{server, <<"localhost">>},
		{password, <<"break_me">>},
		{resource, <<>>}, %% Allow the server to generate the resource
		{auth, {escalus_auth, auth_sasl_external}},
		{starttls, required}],

    Result = escalus_connection:start(UserSpec),
    ?assertMatch({error, {connection_step_failed, _, _}}, Result),
    {error, {connection_step_failed, _Call, Details}} = Result,
    ?assertMatch({auth_failed, _, #xmlel{name = <<"failure">>}}, Details),
    ok.

generate_certs(C) ->
    CA = [#{cn => "not-alice", xmpp_addrs => ["alice@localhost", "alice@fed1"]},
          #{cn => "bob", xmpp_addrs => ["bob@localhost"]},
          #{cn => "john"},
          #{cn => "not-mike"},
          #{cn => "grace", xmpp_addrs => ["grace@fed1", "grace@reg1"]}],
    SelfSigned = [ M#{cn => CN ++ "-self-signed", signed => self} || M = #{ cn := CN } <- CA],
    CertSpecs = CA ++ SelfSigned,
    ct:pal("~p~n", [CertSpecs]),
    Certs = [{maps:get(cn, CertSpec), generate_cert(C, CertSpec)} || CertSpec <- CertSpecs],
    [{certs, maps:from_list(Certs)} | C].

generate_cert(C, #{cn := User} = CertSpec) ->
    ConfigTemplate = filename:join(?config(data_dir, C), "openssl-user.cnf"),
    {ok, Template} = file:read_file(ConfigTemplate),
    XMPPAddrs = maps:get(xmpp_addrs, CertSpec, undefined),
    TemplateValues = prepare_template_values(User, XMPPAddrs),
    OpenSSLConfig = bbmustache:render(Template, TemplateValues),
    UserConfig = filename:join(?config(priv_dir, C), User ++ ".cfg"),
    file:write_file(UserConfig, OpenSSLConfig),
    UserKey = filename:join(?config(priv_dir, C), User ++ "_key.pem"),

    case maps:get(signed, CertSpec, ca) of
	ca ->
	    generate_ca_signed_cert(C, User, UserConfig, UserKey);
	self ->
	    generate_self_signed_cert(C, User, UserConfig, UserKey)
    end.

generate_ca_signed_cert(C, User, UserConfig, UserKey ) ->
    UserCsr = filename:join(?config(priv_dir, C), User ++ ".csr"),

    Cmd = ["openssl", "req", "-config", UserConfig, "-newkey", "rsa:2048", "-sha256", "-nodes",
	   "-out", UserCsr, "-keyout", UserKey, "-outform", "PEM"],
    {done, 0, _Output} = erlsh:run(Cmd),

    UserCert = filename:join(?config(priv_dir, C), User ++ "_cert.pem"),
    SignCmd = filename:join(?config(data_dir, C), "sign_cert.sh"),
    Cmd2 = [SignCmd, "--req", UserCsr, "--out", UserCert],
    LogFile = filename:join(?config(priv_dir, C), User ++ "signing.log"),
    SSLDir = filename:join([path_helper:repo_dir(C), "tools", "ssl"]),
    {done, 0, _} = erlsh:run(Cmd2, LogFile, SSLDir),
    #{key => UserKey,
      cert => UserCert}.

generate_self_signed_cert(C, User, UserConfig, UserKey) ->
    UserCert = filename:join(?config(priv_dir, C), User ++ "_self_signed_cert.pem"),

    Cmd = ["openssl", "req", "-config", UserConfig, "-newkey", "rsa:2048", "-sha256", "-nodes",
	   "-out", UserCert, "-keyout", UserKey, "-x509", "-outform", "PEM", "-extensions", "client_req_extensions"],
    {done, 0, _Output} = erlsh:run(Cmd),
    #{key => UserKey,
      cert => UserCert}.


generate_user_tcp(C, User) ->
    generate_user(C, User, escalus_tcp).

generate_user(C, User, Transport) ->
    Certs = ?config(certs, C),
    UserCert = maps:get(User, Certs),

    Common = [{username, list_to_binary(User)},
	      {server, <<"localhost">>},
	      {password, <<"break_me">>},
	      {resource, <<>>}, %% Allow the server to generate the resource
	      {auth, {escalus_auth, auth_sasl_external}},
	      {transport, Transport},
	      {ssl_opts, [{certfile, maps:get(cert, UserCert)},
			  {keyfile, maps:get(key, UserCert)}]}],
    Common ++ transport_specific_options(Transport).

transport_specific_options(escalus_tcp) ->
    [{starttls, required}];
transport_specific_options(_) ->
     [{port, 5285},
      {ssl, true}].


prepare_template_values(User, XMPPAddrsIn) ->
    Defaults = #{"cn" => User,
		 "xmppAddrs" => ""},
    XMPPAddrs = maybe_prepare_xmpp_addresses(XMPPAddrsIn),
    Defaults#{"xmppAddrs" => XMPPAddrs}.

maybe_prepare_xmpp_addresses(undefined) ->
    "";
maybe_prepare_xmpp_addresses(Addrs) when is_list(Addrs) ->
    AddrsWithSeq = lists:zip(Addrs, lists:seq(1, length(Addrs))),
    Entries = [make_xmpp_addr_entry(Addr, I) || {Addr, I} <- AddrsWithSeq],
    string:join(Entries, "\n").

make_xmpp_addr_entry(Addr, I) ->
    % id-on-xmppAddr OID is specified in the openssl-user.cnf config file
    "otherName." ++ integer_to_list(I) ++ " = id-on-xmppAddr;UTF8:" ++ Addr.

