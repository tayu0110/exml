use std::io;

use clap::Parser;
use exml::nanohttp::xml_nanohttp_fetch;

#[derive(clap::Parser)]
struct Arg {
    url: String,
    #[clap(short = 'O', long = "output-document")]
    output: Option<String>,
}

fn main() -> io::Result<()> {
    let arg = Arg::parse();

    let filename = arg.output.as_deref().unwrap_or("-");
    let mut content_type = None;
    xml_nanohttp_fetch(&arg.url, filename, &mut content_type)?;
    Ok(())
}
