use std::io::BufReader;
use std::path::PathBuf;

use futures::Stream as _;
use log::error;
use structopt::StructOpt;

use gerritbot_gerrit as gerrit;

#[derive(StructOpt, Debug)]
struct Args {
    /// Gerrit username
    #[structopt(short = "u")]
    username: String,
    /// Gerrit hostname
    hostname: String,
    /// Gerrit SSH port
    #[structopt(short = "p", default_value = "29418")]
    port: u32,
    /// Path to SSH private key
    #[structopt(short = "i", parse(from_os_str))]
    private_key_path: PathBuf,
}

fn main() {
    env_logger::init_from_env(
        env_logger::Env::default()
            .filter_or(
                "GERRITBOT_LOG",
                concat!(module_path!(), "=info,gerritbot_gerrit=info"),
            )
    );
    let args = Args::from_args();

    let connection = gerrit::Connection::connect(
        format!("{}:{}", args.hostname, args.port),
        args.username,
        args.private_key_path,
    )
    .unwrap_or_else(|e| {
        error!("connection to gerrit failed: {}", e);
        std::process::exit(1);
    });

    let mut command_runner = gerrit::CommandRunner::new(connection);
    let stdin_lines = tokio::io::lines(BufReader::new(tokio::io::stdin()));

    tokio::run(
        stdin_lines
            .map_err(|e| format!("failed to read line: {}", e))
            .and_then(move |line| command_runner.run_command(format!("gerrit query {}", line)))
            .map_err(|e| error!("error: {}", e))
            .for_each(|output| {
                println!("{}", output);
                Ok(())
            }),
    );

    /*
    tokio::run(
        command_runner
            .run_command(format!("gerrit query {}", args.query))
            .map_err(|e| error!("error running query: {}", e))
            .map(|output| println!("{}", output)),
    );
    */
}
