Grow Beard Configuration Manager

Configuration manager library for erlang systems.

Uses Yaml files to manage configurations.
Base configuration file is `gb_conf.yaml` which defaults its contents from `priv/gb_conf.yaml.default`.
Any umbrella application can place a `gb_conf.yaml` under its own priv dir to overwrite this file.
