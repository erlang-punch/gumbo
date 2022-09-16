%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2022 (c) Mathieu Kerjouan
%%%
%%% @doc gen_server template for gumbo application
%%% @end
%%%-------------------------------------------------------------------
-module(gumbo_server).
-behaviour(gen_server).
-export([start/0, start/1, start/2, start/3]).
-export([start_link/0, start_link/1, start_link/2, start_link/3]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

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
    gen_server:start(?MODULE, Args, Opts).

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
    gen_server:start({local, ?MODULE}, ?MODULE, Args, Opts).

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
    gen_server:start_link(?MODULE, Args, Opts).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: term(),
      Return :: {ok, State} | {ok,State,Timeout} | {ok,State,hibernate},
      Timeout :: integer(),
      State :: term().
init(_Args) ->
    State = [],
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc terminate/2
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Data) ->
    ok.


%%--------------------------------------------------------------------
%% @doc handle_call/3
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      From :: term(),
      Message :: term(),
      State :: term(),
      Return :: term().
handle_call(Message, From, State) ->
    logger:warning("got ~p from ~p", [Message, From]),
    {reply, Message, State}.

%%--------------------------------------------------------------------
%% @doc handle_cast/2
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: term(),
      State :: term(),
      Return :: term().
handle_cast(Message, State) ->
    logger:warning("got ~p", [Message]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc handle_info/2
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: term(),
      State :: term(),
      Return :: term().
handle_info(Message, State) ->
    logger:warning("got ~p", [Message]),
    {noreply, State}.
