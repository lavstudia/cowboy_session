%% ===================================================================
%% Common project options
%% ===================================================================

-define(CHILD(I, Type),
     #{id => I,
        start => {I, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => Type,
        modules => [I]
    }).
-define(CHILD(I, Type, Restart),
     #{id => I,
        start => {I, start_link, []},
        restart => Restart,
        shutdown => 5000,
        type => Type,
        modules => [I]
    }).


%% ===================================================================
%% Cookies options
%% ===================================================================

-define(C_NAME, <<"session">>).
-define(C_OPTIONS, #{path => <<"/">>}).
-define(C_EXPIRE, 1440).
-define(C_STORAGE, cowboy_session_storage_ets).