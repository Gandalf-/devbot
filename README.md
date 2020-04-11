# Devbot

Devbot is a command orchestration tool, similar to `cron`. Actions are sequences of
commands, potentially requiring a condition, that are run at intervals and checked for
errors. Devbot is platform independent, and runs on Linux, Windows, MacOS, and BSD.

Devbot's goals are to be: simple to use, robust, easily inspectable, and lightweight.

```
devbot usage:
  start       - start devbot
  daemon      - start devbot in the background
  stop        - stop devbot and all services

  list        - show a list summary of runtime data and config
  table       - show a table summary of runtime data and config
  status      - give a single character summary of run state

  schema      - show the config file schema
  config      - show the config file path
  parse <exp> - show how devbot will interpet an interval expression
```

# Devbot

Devbot works by loading a config file into an internal database on start up, building a
schedule to match the requirements provided, and waiting for the right time to run each
action.

The internal database contains the current configuration, runtime information, and meta
data. If devbot is interrupted, runtime information is not lost. This means that an
action meant to run in 5 hours will still run on schedule if devbot is stopped for a
hour and started again.

**Events** are periodic tasks. Events do not need to be short lived, but another
instance of the event will not be started if the previous run has not yet completed. The
`action` field may be a string or array of strings, which will be executed by the
system's default shell. On Linux, this is usually `/bin/sh`. On Windows, this is usually
`cmd`.

Except on Windows, each item in an array `action` definition will be executed in a
single shell, like you would expect to happen with a shell or batch script. This allows
usage like the following:
```yaml
- cd $HOME/my-project
- git fetch
- git pull
```

On Linux, devbot will convert this to an equivalent of the following, so you don't have to!
```shell
{ cd $HOME/my-project; } && { git fetch; } && { git pull; }
```

If you want each item in the `action` field to be run simultaneously instead of serially
in a single shell, you can set the `parallel` option to `true`. This example shows how
you can have devbot run several rsync jobs in parallel.
```yaml
events:
  backup-my-stuff:
    action:
      - rsync -av ~/Documents   some-machine:backups/Documents
      - rsync -av ~/Pictures   other-machine:backups/Pictures
      - rsync -av ~/Downloads remote-machine:backups/Downloads
    parallel: true
    interval: daily
```

If an action exits with failure, it's treated as an error and the event will be retried.
Retries backoff exponentially to limit system thrashing when a command cannot
successfully complete. `devbot list` and `devbot table` show events with errors in red.

**Services** are long running tasks, usually other daemons. It's not expected that
services restart or exit. If they do, they will be restarted by devbot. There are many
service management tools and frameworks available; devbot's goal is to be simple and
cross platform. If you want to run `syncthing` on a couple Linux and Windows machines,
the devbot config would be identical:

```yaml
services:
  syncthing:
    action: syncthing -no-browser -verbose
```


## Example configuration
```yaml
events:

  dotfiles-fetch:
    action:
      - cd ~/working/devbot
      - git fetch
    interval: twice per day
    require:  network

  backup-my-stuff:
    action:
      - rsync -av ~/Documents   some-machine:backups/Documents
      - rsync -av ~/Pictures   other-machine:backups/Pictures
      - rsync -av ~/Downloads remote-machine:backups/Downloads
    parallel: true
    interval: every other day
    require:  network

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
dotfiles-fetch
    cd ~/DotFiles
    git fetch
    every 12 hours, took 1 second, requires network, next in 19 hours

backup-my-stuff:
    rsync -av ~/Documents   some-machine:backups/Documents
    rsync -av ~/Pictures   other-machine:backups/Pictures
    rsync -av ~/Downloads remote-machine:backups/Downloads
    every 48 hours, took 3 minutes, requires network, next in 32 hours

service: syncthing
    uptime 5 days, 3 hours
```

# devbot table

Shows another, more condensed, view of the current runtime information.

```
         events         |    every     |     next     |     last     |  require  |  options
                        |              |              |              |           |
        heatbeat        |     hour     |  25 minutes  |   1 second   |           |
     internet-check     |  5 minutes   |   1 minute   |  9 seconds   |           |
  public-files-publish  |   6 hours    |   4 hours    |  47 seconds  |  network  |
     sensor-create      |  15 minutes  |  10 minutes  |  4 seconds   |           |
     sensor-publish     |  15 minutes  |  11 minutes  |  3 seconds   |  network  |    P
      share-zipper      |     day      |   5 hours    |  25 seconds  |           |
    timelapse-create    |     hour     |  25 minutes  |  3 seconds   |           |


  services  |  uptime   |            action
            |           |
  database  |  7 hours  |  apocrypha-server --headless

```

# devbot status

Shows the current state of the devbot daemon as a single character, useful for
embedding in your tmux status line.

- Running  ✓
- Stopped  ✗

# devbot schema

Shows the accepted configuration file schema, for easy reference when making config file
modifications.

# devbot parse

Devbot is very flexible in how it can interpret event intervals. You can use the
`devbot parse` command to test that expressions can be parsed, and verify that they
evaluate to what you expect.

```
$ devbot parse every hour
parsed as: 3600 seconds
listed as: 1 hour

$ devbot parse hourly
parsed as: 3600 seconds
listed as: 1 hour

$ devbot parse every other hour
parsed as: 7200 seconds
listed as: 2 hours

$ devbot parse twice hourly
parsed as: 1800 seconds
listed as: 30 minutes

$ devbot parse twice every 100 years
parsed as: 1451520000 seconds
listed as: 50 years

$ devbot parse 89 times per month
parsed as: 27182 seconds
listed as: 7 hours

$ devbot parse 309 times per week
parsed as: 1957 seconds
listed as: 32 minutes
```
