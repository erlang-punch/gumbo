%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gumbo_driver2).
-compile(export_all).
-behavior(gen_server).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-record(state, { shared_lib_path = "c_src"
               , shared_lib = "gumbo_driver"
               , port = undefined
               , from = undefined
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
    gen_server:start({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Args) ->
    SharedLib = proplists:get_value(shared_lib, Args, "gumbo_driver"),
    SharedLibPath = proplists:get_value(shared_lib_path, Args, "c_src"),
    io:format("start the driver with '~p'~n", [SharedLib]),
    case erl_ddll:load_driver(SharedLibPath, SharedLib) of
        ok ->
            Port = open_port({spawn, SharedLib}, []),
            {ok, #state{ shared_lib = SharedLib
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
stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
gumbo_html_validation(String) ->
    gen_server:call(?MODULE, {valid, String}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
gumbo_html_validation_errors(String) ->
    gen_server:call(?MODULE, {errors, String}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call({html_validation, String}, From, #state{ port = Port } = State) ->
    Port ! {self(), {command, encode({html_validation, String})}},
    {noreply, State#state{ from = From }}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(close, #state{ port = Port } = State) ->
    Port ! {self(), close},
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info({Port, {data, Data}}, #state{ port = Port, from = From } = State) 
  when From =/= undefined ->
    Validation = case decode(Data) of
                     X when X =:= 0 -> true;
                     _ -> false
                 end,
    gen_server:reply(From, Validation),
    {noreply, State#state{ from = undefined }};
handle_info({Port, closed}, #state{ port = Port } = State) ->
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
