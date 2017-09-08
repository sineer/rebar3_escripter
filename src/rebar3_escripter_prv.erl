-module(rebar3_escripter_prv).

-include_lib("kernel/include/file.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar3_escripter).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
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
    Providers = rebar_state:providers(State),
    BaseDir = rebar_state:dir(State),
    rebar_hooks:run_project_and_app_hooks(BaseDir, pre, ?PROVIDER, Providers, State),
    %% Cwd = rebar_state:dir(State),
    %% Path = filename:join([rebar_dir:root_dir(State), "scripts"]),

    Dirs = [filename:join(BaseDir, "apps")],
    RebarOpts = rebar_state:opts(State),
    SrcDirs = rebar_dir:src_dirs(RebarOpts, ["src"]),
    rebar_api:info("Dirs: ~p SrcDirs: ~p", [Dirs, SrcDirs]),
    Apps = rebar_app_discover:find_unbuilt_apps(Dirs, SrcDirs),
    rebar_api:info("APPS: ~p", [Apps]),


    %% rebar_api:info("PATH: ~p", [Path]),
    %% rebar_api:info("Building escript...", []),
    %% Apps = rebar_app_discover:find_unbuilt_apps([Path]),
    %% rebar_api:info("APPS: ~p", [Apps]),
    lists:foreach(fun(App) -> escriptize(State, App) end, Apps),
    {ok, State}.


escriptize(State0, App) ->
    AppName = rebar_app_info:name(App),
    AppNameStr = to_list(AppName),

    %% Get the output filename for the escript -- this may include dirs
    Filename = filename:join([rebar_dir:base_dir(State0), "bin",
                              rebar_state:get(State0, escript_name, AppName)]),
    rebar_api:debug("Creating escript file ~ts", [Filename]),
    ok = filelib:ensure_dir(Filename),
    State = rebar_state:escript_path(State0, Filename),

    %% Look for a list of other applications (dependencies) to include
    %% in the output file. We then use the .app files for each of these
    %% to pull in all the .beam files.
    TopInclApps = lists:usort([ec_cnv:to_atom(AppName) | rebar_state:get(State, escript_incl_apps, [])]),
    AllApps = rebar_state:all_deps(State)++rebar_state:project_apps(State),
    InclApps = find_deps(TopInclApps, AllApps),
    InclBeams = get_apps_beams(InclApps, AllApps),

    %% Look for a list of extra files to include in the output file.
    %% For internal rebar-private use only. Do not use outside rebar.
    InclExtra = get_extra(State),

    %% Construct the archive of everything in ebin/ dir -- put it on the
    %% top-level of the zip file so that code loading works properly.
    EbinPrefix = filename:join(AppNameStr, "ebin"),
    EbinFiles = usort(load_files(EbinPrefix, "*", "ebin")),

    ExtraFiles = usort(InclBeams ++ InclExtra),
    Files = get_nonempty(EbinFiles ++ (ExtraFiles -- EbinFiles)), % drop dupes

    DefaultEmuArgs = lists:flatten(io_lib:format("%%! -escript main ~ts -pz ~ts/~ts/ebin\n",
                                                  [AppNameStr, AppNameStr, AppNameStr])),
    EscriptSections =
        [ {shebang,
           def("#!", State, escript_shebang, "#!/usr/bin/env escript\n")}
        , {comment, def("%%", State, escript_comment, "%%\n")}
        , {emu_args, def("%%!", State, escript_emu_args, DefaultEmuArgs)}
        , {archive, Files, []} ],
    case escript:create(Filename, EscriptSections) of
        ok -> ok;
        {error, EscriptError} ->
            throw({escript_creation_failed, AppName, EscriptError})
    end,

    %% Finally, update executable perms for our script on *nix or write out
    %% script files on win32
    case os:type() of
        {unix, _} ->
            {ok, #file_info{mode = Mode}} = file:read_file_info(Filename),
            ok = file:change_mode(Filename, Mode bor 8#00111);
        {win32, _} ->
            write_windows_script(Filename)
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error({write_failed, AppName, WriteError}) ->
    io_lib:format("Failed to write ~p script: ~p", [AppName, WriteError]);
format_error({zip_error, AppName, ZipError}) ->
    io_lib:format("Failed to construct ~p escript: ~p", [AppName, ZipError]);
format_error({bad_name, App}) ->
    io_lib:format("Failed to get ebin/ directory for "
                   "escript_incl_app: ~p", [App]);
format_error(no_main_app) ->
    io_lib:format("Multiple project apps and {escript_main_app, atom()}."
                 " not set in rebar.config", []).

%% ===================================================================
%% Internal functions
%% ===================================================================

get_apps_beams(Apps, AllApps) ->
    get_apps_beams(Apps, AllApps, []).

get_apps_beams([], _, Acc) ->
    Acc;
get_apps_beams([App | Rest], AllApps, Acc) ->
    case rebar_app_utils:find(to_binary(App), AllApps) of
        {ok, App1} ->
            OutDir = filename:absname(rebar_app_info:ebin_dir(App1)),
            Beams = get_app_beams(App, OutDir),
            get_apps_beams(Rest, AllApps, Beams ++ Acc);
        _->
            case code:lib_dir(App, ebin) of
                {error, bad_name} ->
                    throw({bad_name, App});
                Path ->
                    Beams = get_app_beams(App, Path),
                    get_apps_beams(Rest, AllApps, Beams ++ Acc)
            end
    end.

get_app_beams(App, Path) ->
    Prefix = filename:join(atom_to_list(App), "ebin"),
    load_files(Prefix, "*.beam", Path) ++
        load_files(Prefix, "*.app", Path).

get_extra(State) ->
    Extra = rebar_state:get(State, escript_incl_extra, []),
    lists:foldl(fun({Wildcard, Dir}, Files) ->
                        load_files(Wildcard, Dir) ++ Files
                end, [], Extra).

load_files(Wildcard, Dir) ->
    load_files("", Wildcard, Dir).

load_files(Prefix, Wildcard, Dir) ->
    [read_file(Prefix, Filename, Dir)
     || Filename <- filelib:wildcard(Wildcard, Dir)].

read_file(Prefix, Filename, Dir) ->
    Filename1 = case Prefix of
                    "" ->
                        Filename;
                    _ ->
                        filename:join([Prefix, Filename])
                end,
    [dir_entries(filename:dirname(Filename1)),
     {Filename1, file_contents(filename:join(Dir, Filename))}].

file_contents(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Bin.

%% Given a filename, return zip archive dir entries for each sub-dir.
%% Required to work around issues fixed in OTP-10071.
dir_entries(File) ->
    Dirs = dirs(File),
    [{Dir ++ "/", <<>>} || Dir <- Dirs].

%% Given "foo/bar/baz", return ["foo", "foo/bar", "foo/bar/baz"].
dirs(Dir) ->
    dirs1(filename:split(Dir), "", []).

dirs1([], _, Acc) ->
    lists:reverse(Acc);
dirs1([H|T], "", []) ->
    dirs1(T, H, [H]);
dirs1([H|T], Last, Acc) ->
    Dir = filename:join(Last, H),
    dirs1(T, Dir, [Dir|Acc]).

usort(List) ->
    lists:ukeysort(1, lists:flatten(List)).

get_nonempty(Files) ->
    [{FName,FBin} || {FName,FBin} <- Files, FBin =/= <<>>].

find_deps(AppNames, AllApps) ->
    BinAppNames = [to_binary(Name) || Name <- AppNames],
    [ec_cnv:to_atom(Name) ||
     Name <- find_deps_of_deps(BinAppNames, AllApps, BinAppNames)].

%% Should look at the app files to find direct dependencies
find_deps_of_deps([], _, Acc) -> Acc;
find_deps_of_deps([Name|Names], Apps, Acc) ->
    rebar_api:debug("processing ~p", [Name]),
    {ok, App} = rebar_app_utils:find(Name, Apps),
    DepNames = proplists:get_value(applications, rebar_app_info:app_details(App), []),
    BinDepNames = [to_binary(Dep) || Dep <- DepNames,
                   %% ignore system libs; shouldn't include them.
                   DepDir <- [code:lib_dir(Dep)],
                   DepDir =:= {error, bad_name} orelse % those are all local
                   not lists:prefix(code:root_dir(), DepDir)]
                -- ([Name|Names]++Acc), % avoid already seen deps
    rebar_api:debug("new deps of ~p found to be ~p", [Name, BinDepNames]),
    find_deps_of_deps(BinDepNames ++ Names, Apps, BinDepNames ++ Acc).

def(Rm, State, Key, Default) ->
    Value0 = rebar_state:get(State, Key, Default),
    case Rm of
        "#!"  -> "#!"  ++ Value = Value0, rm_newline(Value);
        "%%"  -> "%%"  ++ Value = Value0, rm_newline(Value);
        "%%!" -> "%%!" ++ Value = Value0, rm_newline(Value)
    end.

rm_newline(String) ->
    [C || C <- String, C =/= $\n].

write_windows_script(Target) ->
    CmdPath = if is_binary(Target) -> <<Target/binary, ".cmd">>;
                 is_list(Target) -> Target ++ ".cmd"
              end,
    CmdScript=
        "@echo off\r\n"
        "setlocal\r\n"
        "set rebarscript=%~f0\r\n"
        "escript.exe \"%rebarscript:.cmd=%\" %*\r\n",
    ok = file:write_file(CmdPath, CmdScript).

to_binary(A) when is_atom(A) -> atom_to_binary(A, unicode);
to_binary(Str) -> unicode:characters_to_binary(Str).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(Str) -> unicode:characters_to_list(Str).
