-module(mod_percept2_ws).

-export([init/3, websocket_init/3, websocket_info/3,
         websocket_handle/3]).


init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_Type, Req, _Opts) ->
    {ok, Req, {nil, nil}}.


websocket_info({Pid, analyzing}, Req, {_, Pid}=State) ->
    Msg = {[{<<"status">>, <<"analyzing">>}]},
    {reply, {text, jiffy:encode(Msg)}, Req, State};
websocket_info({Pid, done}, Req, {Name, Pid}=State) ->
    Msg = {[{<<"status">>, <<"done">>},
            {<<"name">>, Name}]},
    {reply, {text, jiffy:encode(Msg)}, Req, State};
websocket_info(_Info, Req, State) ->
    io:format("got else ~p~n", [_Info]),
    {ok, Req, State}.


websocket_handle({text, Txt}, Req, State) ->
    {Props} = jiffy:decode(Txt),
    io:format("got ~p~n", [Props]),
    case proplists:get_value(<<"cmd">>, Props) of
        <<"profile">> ->
            Mods = proplists:get_value(<<"mods">>, Props, []),
            Name =  proplists:get_value(<<"name">>, Props),
            case mod_percept2:start_profile(binary_to_list(Name), Mods, self()) of
                {ok, Pid} ->
                    Msg = {[{<<"ok">>, true}]},
                    {reply, {text, jiffy:encode(Msg)}, Req, {Name, Pid}};
                {arleady_started, {CurrentName, CurrentMods}} ->
                    Msg = {[{<<"error">>, <<"already_started">>},
                            {<<"name">>, CurrentName},
                            {<<"mods">>, CurrentMods}]},
                    {close, {text, jiffy:encode(Msg)}, Req, State};
                Else ->
                    {ok, Req, State}

            end;
        _ ->
            io:format("damn ~n", []),
            Msg = {[{<<"error">>, <<"invalid_cmd">>}]},
            {close, {text, jiffy:encode(Msg)}, Req, State}
    end;
websocket_handle(_Msg, Req, State) ->
    {ok, Req, State}.
