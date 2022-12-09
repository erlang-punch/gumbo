-module(gumbo_driver).
-compile(export_all).

start() ->
    start("gumbo_driver").

start(SharedLib) ->
    io:format("start the driver with '~p'~n", [SharedLib]),
    case erl_ddll:load_driver("c_src", SharedLib) of
        ok -> 
            ok;
        {error, already_loaded} -> 
            ok;
        _ -> 
            exit({error, could_not_load_driver})
    end,
    spawn(?MODULE, init, [SharedLib]).

init(SharedLib) ->
    register(gumbo_driver, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    gumbo_driver ! stop.

gumbo_html_validation(String) ->
    call_port({html_validation, String}).

call_port(Msg) ->
    gumbo_driver ! {call, self(), Msg},
    receive
        {html_validation, Result} ->
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            io:format("call: ~p:~p~n", [Caller, Msg]),
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    io:format("data: ~p~n", [Data]),
                    Caller ! {html_validation, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
            exit(port_terminated);
        {debug, Message} ->
            io:format("received: ~p~n", [Message]),
            loop(Port)
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% encode/1 function will send a value from the node to the driver.
%% @end
%%-----------------------------------------------------------------------------
encode({html_validation, String}) -> String.

%%--------------------------------------------------------------------
%% @doc
%% decode/1 function decode the value from the driver.
%% @end
%%--------------------------------------------------------------------
decode([0]) -> true;
decode([X]) -> {false, X};
decode(Anything) -> Anything.

