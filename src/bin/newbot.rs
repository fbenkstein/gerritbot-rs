// use futures::stream::Stream as _;

use gerritbot::gerrit;

mod args;

fn main() -> Result<(), String> {
    let args = args::parse_args();
    stderrlog::new()
        .module(module_path!())
        .quiet(args.flag_quiet)
        .timestamp(stderrlog::Timestamp::Second)
        .verbosity(if args.flag_verbose { 5 } else { 2 })
        .init()
        .unwrap();
    let args::Config {
        gerrit: gerrit_config,
        ..
    } = args::parse_config(args.flag_config);

    let gerrit = gerrit::Gerrit::new(
        gerrit_config.host,
        gerrit_config.username,
        gerrit_config.priv_key_path,
    )?;

    let _events = gerrit.events();

    /*
    tokio::run(events.for_each(|event| {
        println!("gerrit event: {:#?}", event);
        Ok(())
    }))
    */
    Ok(())
}
