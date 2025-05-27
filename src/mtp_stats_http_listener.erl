%%% @doc Cowboy HTTP listener for /stats endpoint.
-module(mtp_stats_http_listener).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/2, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/stats", mtp_stats_http_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(stats_http_listener,
                                [{port, 8080}],
                                #{env => #{dispatch => Dispatch}}),
    {ok, #{}}.

handle_call(_Request, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
