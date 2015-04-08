-module(mod_websocket_turn).

-behaviour(gen_mod).
-behaviour(cowboy_websocket_handler).

-include("ejabberd.hrl").

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_websocket_handler callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
start(_Host, _Opts) ->
    ?INFO_MSG("module ~p started", [?MODULE]),
    ok.

stop(_Host) ->
    ?INFO_MSG("module ~p stopped", [?MODULE]),
    ok.

%%--------------------------------------------------------------------
%% cowboy_websocket_handler callbacks
%%--------------------------------------------------------------------
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Transport, Req, _Opts) ->
    ?INFO_MSG("~p handler initialized in ~p", [?MODULE, self()]),
    {ok, Req, no_state}.

websocket_handle(Data, Req, _State) ->
    ?INFO_MSG("~p websocket_handle called with req ~p (~p)",
              [?MODULE, Data, self()]),
    {ok, Req, no_state}.

websocket_info(Message, Req, _State) ->
    ?INFO_MSG("~p websocket_info called with message ~p (~p)",
              [?MODULE, Message, self()]),
    {ok, Req, no_state}.

websocket_terminate(Reason, _Req, _State) ->
    ?INFO_MSG("~p websocket_terminate called with reason ~p (~p)",
              [?MODULE, Reason, self()]),
    ok.
