-type island() :: [mas:agent()].

-type counter() :: dict:dict().

-type model() :: mas_sequential | mas_hybrid | mas_concurrent | mas_skel.

-type funstat() :: {Name::atom(), Map::fun(), Reduce::fun(), Value::term()}.

-record(config, {model :: model(),
                 agent_env :: atom(),
                 topology :: atom(),
                 log_dir :: atom() | string(),
                 islands :: pos_integer(),
                 population_size :: pos_integer(),
                 migration_probability :: float(),
                 write_interval :: pos_integer(),
                 skel_workers :: pos_integer(),
                 arena_timeout :: pos_integer(),
                 skel_split_size :: pos_integer(),
                 skel_pull :: atom()}).

-type config() :: #config{}.
