-module(erliam_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%% API

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%%% SUPERVISOR CALLBACKS

init(_) ->
    configure_httpc_profile(),

    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},

    CredSrv = #{id => erliam_srv,
                type => worker,
                shutdown => 5000,
                start => {erliam_srv, start_link, []}},
    {ok, {SupFlags, [CredSrv]}}.

%%%% INTERNAL FUNCTIONS

configure_httpc_profile() ->
    Profile = erliam:httpc_profile(),
    inets:start(httpc, [{profile, Profile}]),
    ok = httpc:set_options([{max_sessions, 8},
                            {max_keep_alive_length, 0},
                            {max_pipeline_length, 0},
                            {keep_alive_timeout, 1000}],
                           Profile).

