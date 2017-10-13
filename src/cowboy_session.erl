-module(cowboy_session).

%% API
-export([
    start/0,
    on_request/1,
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

-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req) ->
    {_Session, Req2} = get_session(Req),
    Req2.

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
    supervisor:start_child(Sup, ?CHILD(?C_STORAGE, worker)),
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
    Cookie_name = ?C_NAME,
    CookieNameAtom = erlang:binary_to_atom(Cookie_name, unicode),
    #{CookieNameAtom := SID} = cowboy_req:match_cookies([{CookieNameAtom, [], undefined}], Req),
    case SID of
        undefined ->
            create_session(Req);
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
    Cookie_name = ?C_NAME,
    cowboy_req:set_resp_cookie(Cookie_name, <<"deleted">>, Req, #{max_age => 0}).

create_session(Req) ->
    %% The cookie value cannot contain any of the following characters:
    %%   ,; \t\r\n\013\014
    SID = list_to_binary(uuid:to_string(uuid:v4())),
    Cookie_name = ?C_NAME,
    Cookie_options = ?C_OPTIONS,
    Storage = ?C_STORAGE,
    Expire = ?C_EXPIRE,
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