-module(cowboy_session).

%% API
-export([
    start/0,
    execute/2,
    get/2, get/3,
    set/3,
    expire/1,
    touch/1
]).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

%% Config
-include("cowboy_session_config.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec start() -> ok.
start() ->
    ensure_started([?MODULE]).

-spec execute(cowboy_req:req(), cowboy_middleware:env()) -> {ok, cowboy_req:req(), cowboy_middleware:env()}
                                                            | {suspend, module(), atom(), [any()]}
                                                            | {stop, cowboy_req:req()}.
execute(Req, Env) ->    
    {_, Req1} = get_session(Req),
    {ok, Req1, Env}
.


get(Key, Req) ->
    get(Key, undefined, Req).

get(Key, Default, Req) ->
    {Pid, Req2} = get_session(Req),
    Value = cowboy_session_server:get(Pid, Key, Default),
    {Value, Req2}.

set(Key, Value, Req) ->
    {Pid, Req2} = get_session(Req),
    cowboy_session_server:set(Pid, Key, Value),
    {ok, Req2}.

expire(Req) ->
    {Pid, Req2} = get_session(Req),
    cowboy_session_server:stop(Pid),
    Req3 = clear_cookie(Req2),
    {ok, Req3}.

touch(Req) ->
    {_Pid, Req2} = get_session(Req),
    {ok, Req2}.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    supervisor:start_child(Sup, ?CHILD(?CONFIG(storage, cowboy_session_storage_ets), worker)),
    {ok, Sup}.

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Restart_strategy = {one_for_one, 10, 5},
    Children = [
        ?CHILD(cowboy_session_server_sup, supervisor)
    ],
    {ok, {Restart_strategy, Children}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_session(Req) ->
    Cookie_name = ?CONFIG(session, <<"session">>),
    Cookies = cowboy_req:parse_cookies(Req),
    SID = proplists:get_value(Cookie_name, Cookies, undefined),

    case SID of
        undefined ->
            create_session(Req, SID);
        _ ->
            case gproc:lookup_local_name({cowboy_session, SID}) of
                undefined ->
                    create_session(Req);
                Pid ->
                    cowboy_session_server:touch(Pid),
                    {Pid, Req}
            end
    end.

clear_cookie(Req) ->
    Cookie_name = ?CONFIG(session, <<"session">>),
    cowboy_req:set_resp_cookie(Cookie_name, <<"deleted">>, Req, #{max_age => 0}).

create_session(Req) ->
    SID = list_to_binary( uuid:uuid_to_string(uuid:get_v4())),
    %% SID = list_to_binary(uuid:to_string(uuid:v4()))
    create_session(Req, SID).

create_session(Req, SID) ->
    Cookie_name = ?CONFIG(session, <<"session">>),
    Cookie_options = ?CONFIG(options, #{path => <<"/">>}),
    Storage = ?CONFIG(storage, cowboy_session_storage_ets),
    Expire = ?CONFIG(expire, 1440),
    {ok, Pid} = supervisor:start_child(cowboy_session_server_sup, [[
        {sid, SID},
        {storage, Storage},
        {expire, Expire}
    ]]),
    Req1 = cowboy_req:set_resp_cookie(Cookie_name, SID, Req, Cookie_options),
    {Pid, Req1}.

ensure_started([]) -> ok;
ensure_started([App | Rest] = Apps) ->
    case application:start(App) of
        ok -> ensure_started(Rest);
        {error, {already_started, App}} -> ensure_started(Rest);
        {error, {not_started, Dependency}} -> ensure_started([Dependency | Apps])
    end.
