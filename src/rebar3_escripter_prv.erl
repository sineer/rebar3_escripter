-module(rebar3_escripter_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar3_escripter).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar_api:info("rebar_escripter_prv State: ~p", [State]),

    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar3_escripter"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "rebar3 plugin to escriptize all folders under scripts/"},
            {desc, "rebar3 plugin to escriptize all folders under scripts/"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    lists:foreach(fun(App) -> escriptize_app(State, App) end, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

escriptize_app(State, App) ->
    rebar_api:info("Escriptizing App: ~p", [App]),
    rebar_prv_escriptize:escriptize(State, App).
