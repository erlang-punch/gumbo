%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact@steepath.eu>
%%% @copyright 2022 (c) Mathieu Kerjouan
%%%
%%% @doc gumbo main application
%%% @end
%%%-------------------------------------------------------------------
-module(gumbo_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc start/2
%% @end
%%--------------------------------------------------------------------
-spec start(StartType, StartArgs) -> Return when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Return :: pid().
start(_StartType, _StartArgs) ->
    gumbo_sup:start_link().

%%--------------------------------------------------------------------
%% @doc stop/1
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> Return when
      State :: term(),
      Return :: ok.
stop(_State) ->
    ok.
