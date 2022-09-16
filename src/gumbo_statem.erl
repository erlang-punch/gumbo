%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2022 (c) Mathieu Kerjouan
%%%
%%% @doc gen_statem template for gumbo application
%%% @end
%%%-------------------------------------------------------------------
-module(gumbo_statem).
-behaviour(gen_statem).
-export([start/0, start/1, start/2, start/3]).
-export([start_link/0, start_link/1, start_link/2, start_link/3]).
-export([callback_mode/0, init/1, terminate/3]).
-export([on/3, off/3]).

%%--------------------------------------------------------------------
%% @doc start/0
%% @end
%%--------------------------------------------------------------------
-spec start() -> Return when
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start() ->
    start([]).

%%--------------------------------------------------------------------
%% @doc start/1
%% @end
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: term(),
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start(Args) ->
    start(Args, []).

%%--------------------------------------------------------------------
%% @doc start/2
%% @end
%%--------------------------------------------------------------------
-spec start(Args, Opts) -> Return when
      Args :: term(),
      Opts :: [term()],
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start(Args, Opts) ->
    gen_statem:start(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc start/3
%% @end
%%--------------------------------------------------------------------
-spec start(Name, Args, Opts) -> Return when
      Name :: atom(),
      Args :: term(),
      Opts :: [term()],
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start(Name, Args, Opts) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc start_link/0
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> Return when
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: term(),
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args, Opts) -> Return when
      Args :: term(),
      Opts :: [term()],
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start_link(Args, Opts) ->
    gen_statem:start_link(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc start_link/3
%% @end
%%--------------------------------------------------------------------
-spec start_link(Name, Args, Opts) -> Return when
      Name :: atom(),
      Args :: term(),
      Opts :: [term()],
      Return :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start_link(Name, Args, Opts) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc callback_mode/0
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> Return when 
      Return :: atom() | [atom()].
callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: term(),
      Return :: {ok, State, Data},
      State :: term(),
      Data :: atom().
init(_Args) ->
    State = on,
    Data = [],
    {ok, State, Data}.

%%--------------------------------------------------------------------
%% @doc terminate/3
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State, _Data) ->
    ok.

%%--------------------------------------------------------------------
%% @doc on/3
%% @end
%%--------------------------------------------------------------------
on(MessageType, Message, Data) ->
    logger:warning("got ~p from ~p", [Message, MessageType]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc off/3
%% @end
%%--------------------------------------------------------------------
off(MessageType, Message, Data) ->
    logger:warning("got ~p from ~p", [Message, MessageType]),
    {keep_state, Data}.
