-define(EXPIRE, shutdown).

%% ===================================================================
%% Common project options
%% ===================================================================

-define(CHILD(I, Type),
     #{id => I,
        start    => {I, start_link, []},
        restart  => permanent,
        shutdown => 5000,
        type     => Type,
        modules  => [I]
    }).
-define(CHILD(I, Type, Restart),
     #{id => I,
        start    => {I, start_link, []},
        restart  => Restart,
        shutdown => 5000,
        type     => Type,
        modules  => [I]
    }).


%% ===================================================================
%% Cookies options
%% ===================================================================

-define(CONFIG(Key, Default), 
    application:get_env(cowboy_session, Key, Default)).
