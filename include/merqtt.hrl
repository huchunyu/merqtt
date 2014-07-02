-define(CONFIG_PATH, "etc/merqtt.cfg").

-define(CONSOLE(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO(Format, Args),
    lager:info(Format, Args)).

-define(WARN(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL(Format, Args),
    lager:critical(Format, Args)).
