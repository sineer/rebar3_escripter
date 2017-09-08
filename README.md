rebar3_escripter
=====

rebar3 plugin to escriptize all folders under scripts/

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_escripter, ".*", {git, "git@host:user/rebar3_escripter.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_escripter
    ===> Fetching rebar3_escripter
    ===> Compiling rebar3_escripter
    <Plugin Output>
