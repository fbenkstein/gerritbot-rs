use std::fs::File;
use std::path::PathBuf;

use docopt::Docopt;
use log::debug;
use rusoto_core::Region;
use serde::Deserialize;

#[derive(Debug, Deserialize, Clone)]
pub struct Args {
    pub flag_verbose: bool,
    pub flag_quiet: bool,
    pub flag_config: PathBuf,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Config {
    pub gerrit: GerritConfig,
    pub spark: SparkConfig,
    pub bot: BotConfig,
}

#[derive(Debug, Deserialize, Clone)]
pub struct GerritConfig {
    pub host: String,
    pub username: String,
    pub priv_key_path: PathBuf,
}

#[derive(Debug, Deserialize, Clone)]
pub struct SparkConfig {
    pub bot_token: String,
    pub api_uri: String,
    pub webhook_url: String,
    pub mode: ModeConfig,
}

#[derive(Debug, Deserialize, Clone)]
pub enum ModeConfig {
    Direct { endpoint: std::net::SocketAddr },
    Sqs { uri: String, region: Region },
}

#[derive(Debug, Deserialize, Clone)]
pub struct BotConfig {
    pub msg_expiration: u64,
    pub msg_capacity: usize,
    pub format_script: Option<String>,
}

const USAGE: &str = "
Cisco Spark <> Gerrit Bot

Usage:
    gerritbot-rs [--verbose | --quiet] --config=<PATH>
    gerritbot-rs [--help]

    -h --help     Show this screen.
    -v --verbose  Print more
    -q --quiet    Be silent

    -c --config=<PATH>   YAML configuration file [default: config.yml]
";

pub fn parse_args() -> Args {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    debug!("{:#?}", args);
    args
}

pub fn parse_config(path: PathBuf) -> Config {
    let file = File::open(path).unwrap_or_else(|e| {
        eprintln!("Could not open config file: {}", e);
        eprintln!("{}", USAGE);
        ::std::process::exit(1)
    });
    let mut config: Config = serde_yaml::from_reader(file).unwrap_or_else(|e| {
        eprintln!("Could not parse config file: {}", e);
        ::std::process::exit(2)
    });
    // tilde expand the private key path
    config.gerrit.priv_key_path =
        shellexpand::tilde(&config.gerrit.priv_key_path.to_string_lossy())
            .into_owned()
            .into();
    debug!("{:#?}", config);
    config
}