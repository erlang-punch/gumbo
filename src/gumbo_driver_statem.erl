%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gumbo_driver_statem).
-compile(export_all).
-include_lib("kernel/include/logger.hrl").
-behavior(gen_statem).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-record(state, { shared_lib_path = "c_src"
               , shared_lib = "gumbo_driver"
               , port = undefined
               , from = undefined
               , command = undefined
               }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-define(SHARED_LIB_PATH, "c_src").
-define(SHARED_LIB, "gumbo_driver").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start() ->
    start([{shared_lib_path, ?SHARED_LIB_PATH}
          ,{shared_lib, ?SHARED_LIB}
          ]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start(Args) ->
    start(Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start(Args, Opts) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
stop() ->
    gen_statem:stop(?MODULE).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Args) ->
    logger:set_module_level(?MODULE, all),
    SharedLib = proplists:get_value(shared_lib, Args, "gumbo_driver"),
    SharedLibPath = proplists:get_value(shared_lib_path, Args, "c_src"),
    logger:info("start the driver with '~p'~n", [SharedLib]),
    case erl_ddll:load_driver(SharedLibPath, SharedLib) of
        ok ->
            Port = open_port({spawn, SharedLib}, []),
            {ok, waiting, #state{ shared_lib = SharedLib
                             , shared_lib_path = SharedLibPath
                             , port = Port
                             }};
        {error, already_loaded} ->
            {stop, {error, already_started}};
        _ ->
            {stop, {error, could_not_load_driver}}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
gumbo_html_validation(String) ->
    gen_statem:call(?MODULE, {html_validation, String}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
gumbo_html_validation_errors(String) ->
    gen_statem:call(?MODULE, {html_validation_errors, String}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
waiting(cast = EventType, close = Event, #state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Port ! {self(), close},
    {next_state, closing, State};
waiting({call, From} = EventType, {html_validation, String} = Event, #state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Port ! {self(), {command, encode({html_validation, String})}},
    {next_state, processing, State#state{ from = From, command = html_validation }};
waiting({call, From} = EventType, {html_validation_errors, String} = Event, #state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Port ! {self(), {command, encode({html_validation_errors, String})}},
    {next_state, processing, State#state{ from = From, command = html_validation_errors }}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
closing(info = EventType, {Port, closed} = Event, #state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    {stop, closed, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
processing(info = EventType, {Port, {data, Data}} = Event, #state{ port = Port, from = From, command = html_validation } = State)
  when From =/= undefined ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Validation = case decode(Data) of
                     X when X =:= 0 -> true;
                     _ -> false
                 end,
    gen_statem:reply(From, Validation),
    {next_state, waiting, State#state{ from = undefined, command = undefined }};
processing(info = EventType, {Port, {data, Data}} = Event, #state{ port = Port, from = From, command = html_validation_errors } = State)
  when From =/= undefined ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Value = decode(Data),
    gen_statem:reply(From, Value),
    {next_state, waiting, State#state{ from = undefined, command = undefined }}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_event(cast = EventType, close = Event, waiting, #state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Port ! {self(), close},
    {next_state, closing, State};

handle_event({call, From} = EventType, {html_validation, String} = Event, waiting, #state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Port ! {self(), {command, encode({html_validation, String})}},
    {next_state, {processing, html_validation}, State#state{ from = From }};

handle_event({call, From} = EventType, {html_validation_errors, String} = Event, waiting, #state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Port ! {self(), {command, encode({html_validation_errors, String})}},
    {next_state, {processing, html_validation_errors}, State#state{ from = From }};

handle_event(info = EventType
            ,{Port, {data, Data}} = Event
            ,{processing, html_validation}
            ,#state{ port = Port, from = From } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Validation = case decode(Data) of
                     X when X =:= 0 -> true;
                     _ -> false
                 end,
    gen_statem:reply(From, Validation),
    {next_state, waiting, State#state{ from = undefined }};

handle_event(info = EventType
            ,{Port, {data, Data}} = Event
            ,{processing, html_validation_errors}
            ,#state{ port = Port, from = From } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    Value = decode(Data),
    gen_statem:reply(From, Value),
    {next_state, waiting, State#state{ from = undefined }};
    
handle_event(info = EventType
            ,{Port, closed} = Event
            ,closing
            ,#state{ port = Port } = State) ->
    ?LOG_DEBUG("~p", [{EventType, Event, State}]),
    {stop, closed, State}.
    

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------------------
%% @doc
%% encode/1 function will send a value from the node to the driver.
%% @end
%%-----------------------------------------------------------------------------
encode({html_validation, String}) -> String;
encode({html_validation_errors, String}) -> String.

%%--------------------------------------------------------------------
%% @doc
%% decode/1 function decode the value from the driver.
%% @end
%%--------------------------------------------------------------------
% TODO: define an header, integer type could use another size than 32bits.
decode([A,B,C,D]) ->
    RawValue = <<A,B,C,D>>,
    <<Integer:32/little-signed-integer>> = RawValue,
    Integer;
decode(Anything) -> {error, Anything}.
