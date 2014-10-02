-module(mod_percept2).
-author('benoitc@benoitcnetwork.eu').

-behaviour(gen_mod).

-export([start/2, stop/1]).
-export([cowboy_router_paths/2]).

-export([start_profile/3]).

-define(WS_LISTENER, mod_percept2_ws).
-define(DEFAULT_PORT, 5281).
-define(PROFILE_DELAY, 30000).



-spec start(ejabberd:server(), list()) -> ok.
start(_Host, Opts) ->
    application:start(gproc),

    Port = gen_mod:get_opt(port, Opts, ?DEFAULT_PORT),
    {started, _, Port} = percept2:start_webserver(Port),
    ok.

-spec stop(ejabberd:server()) -> ok.
stop(_Host) ->
    %% make sure we stop any running profiling
    case whereis(percept2_port) of
        undefined -> ok;
        _ -> do_stop()
    end,
    %% stop the web server
    ok = percept2:stop_webserver(),
    ok.

cowboy_router_paths(BasePath, _Opts) ->
    [
        {[BasePath, "/"], cowboy_static,
         {priv_file, ?MODULE, "static/index.html"}},
        {[BasePath, "/assets/[...]"], cowboy_static,
         {priv_dir, ?MODULE, "static/assets"}},
        {[BasePath, "/ws"], ?WS_LISTENER, []}
    ].

start_profile(Name, Mods, Parent) ->
    Options = case Mods of
        [] -> [all];
        [<<>>] -> [all];
        _ ->
            Mods1 = [list_to_atom(binary_to_list(M)) || M <- Mods, M /= <<>>],
            [all, {callgraph, Mods1}]
    end,
    io:format("Profile with options ~p~n", [Options]),

    % start profiling
    case percept2:profile(Name, Options) of
        {ok, _Port} ->
            Pid = spawn(fun() -> wait_loop(Name, Mods, Parent) end),
            {ok, Pid};
        {already_started, _Port} ->
            WaitPid =  whereis(mod_percept2_loop),
            WaitPid ! {get_info, self()},
            receive
                {WaitPid, info, Info} ->
                    {already_started, Info}
            end
    end.


%% private

do_stop() ->
    %% stop profiling
    percept2:stop_profile(),

    %% exit the loop if it's still runnning
    case whereis(mod_percept2_loop) of
        undefined -> ok;
        Pid ->
            catch exit(Pid, shutdown),
            ok
    end.

wait_loop(Name, Mods, Parent) ->
    %% refister ourself
    erlang:register(mod_percept2_loop, self()),
    %% wait a couple of seconds to collect data
    erlang:send_after(?PROFILE_DELAY, self(), do_analyze),
    receive
        do_analyze ->
            ok = percept2:stop_profile(),
            %% tell to the clients that we started to analyze the data
            Parent ! {self(), analyzing},
            percept2:analyze([Name]),
            %% notify the clients we are done
            Parent ! {self(), done};
        {get_info, From} ->
            From ! {self(), info, {Name, Mods}},
            wait_loop(Name, Mods, Parent)
    end.
