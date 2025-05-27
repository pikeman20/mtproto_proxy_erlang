%%% @doc Cowboy handler for /stats endpoint.
-module(mtp_stats_http_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Accept = cowboy_req:header(<<"accept">>, Req, <<"text/html">>),
            Stats = mtp_metric:passive_metrics(),
            Uptime = mtproto_proxy_app:get_uptime(),
            case Accept of
                <<"application/json">> ->
                    Body = encode_json(Stats, Uptime),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Body, Req),
                    {ok, Req2, State};
                _ ->
                    Body = encode_html(Stats, Uptime),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"text/html">>},
                        Body, Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, Req),
            {ok, Req2, State}
    end.

encode_json(Stats, Uptime) ->
    MapStats = stats_to_map(Stats),
    jsx:encode(#{stats => MapStats, uptime_seconds => Uptime}).

encode_html(Stats, Uptime) ->
    StatsRows = lists:map(fun({Type, Name, Doc, Values}) ->
        io_lib:format("<tr><td>~p</td><td>~p</td><td>~s</td><td>~p</td></tr>", [Type, Name, Doc, Values])
    end, Stats),
    StatsTable = ["<table border='1'><tr><th>Type</th><th>Name</th><th>Doc</th><th>Values</th></tr>" | StatsRows] ++ ["</table>"],
    UptimeStr = format_uptime(Uptime),
    UptimeRow = io_lib:format("<p>Uptime: ~s</p>", [UptimeStr]),
    iolist_to_binary([UptimeRow, StatsTable]).

format_uptime(Secs) when is_integer(Secs) ->
    Days = Secs div 86400,
    Hours = (Secs rem 86400) div 3600,
    Minutes = (Secs rem 3600) div 60,
    Seconds = Secs rem 60,
    case Days of
        0 -> io_lib:format("~2..0B:~2..0B:~2..0B", [Hours, Minutes, Seconds]);
        _ -> io_lib:format("~w days ~2..0B:~2..0B:~2..0B", [Days, Hours, Minutes, Seconds])
    end.

stats_to_map(Stats) ->
    lists:map(fun({Type, Name, Doc, Values}) ->
        #{type => Type, name => Name, doc => Doc, values => Values}
    end, Stats).
