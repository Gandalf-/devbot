# Devbot

Devbot is a command orchestration tool, similar to `cron`. Actions are
sequences of commands, potentially requiring a condition to to run, that are
run at intervals and checked for errors. Devbot is platform independent, and runs on
Linux, Windows, MacOS, and BSD.

Devbot's goals are to be: simple to use, robust, easily inspectable, and lightweight.

```
devbot usage:
  start       - start the devbot daemon

  list        - show a summary of runtime data and config
  status      - give a single character summary of run state

  schema      - show the config file schema
  config      - show the config file path
```

# Devbot

Devbot works by loading a config file into an internal database on start up, builds a
schedule to match the requirements provided, and starts waiting for the right time to
run.

The internal database contains the current configuration, runtime information, and meta
data. If devbot is interrupted, runtime information is not lost. This means that an action meant
to run in 5 hours will still run on schedule if devbot is stopped for a hour and started
again.

**Events** are periodic tasks. Events do not need to be short lived, and another
instance of the event will not be started if the previous run has not yet completed. The
`action` field may be a string or array of strings, which will be executed by the
system's default shell. On Linux, this is usually `/bin/sh`. On Windows, this is usually
`cmd`.

Except on Windows, each item in an array `action` definition will be executed in a
single shell. This allows usage like the following:
```
- cd $HOME/my-project
- git fetch
- git pull
```

If an event exits with failure, it's treated as an error and the event will be retried.
Retries backoff exponentially to limit system thrashing when a command cannot
successfully complete. `devbot status` shows events with errors in red.

**Services** are long running tasks, usually other daemons. It's not expected that
services restart or exit. If they do, they will be restarted by devbot.


## Example configuration
```
events:

  dotfiles-fetch:
    action:
      - cd ~/working/devbot
      - git fetch
    interval: twice per day
    require: network

services:

  syncthing:
    action: syncthing

requirements:
  network: nc -w 1 -z 8.8.8.8 53
```


# devbot list

Shows the current state of all actions, how often they run, any errors, any
requirements, what commands are run, and the next time to run.

```
$ devbot_list

dotfiles-fetch
    cd ~/DotFiles
    git fetch
    every 12 hours, took 1 second, requires network, next in 19 hours

service: syncthing
    uptime 5 days, 3 hours
```

# devbot status

Shows the current state of the devbot daemon as a single character, useful for
embedding in your tmux status line.

- Running  ✓
- Stopped  ✗
