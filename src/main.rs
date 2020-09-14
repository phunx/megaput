use std::error::Error;
use std::process;

use serde::Deserialize;
use enigo::{Enigo, Key, KeyboardControllable, MouseControllable, MouseButton};
use std::fmt;

use hyper::service::{make_service_fn, service_fn};
use hyper::{header, Body, Method, Request, Response, Server, StatusCode};
use serde_json::json;
use clap::{Arg, App};

type GenericError = Box<dyn std::error::Error + Send + Sync>;
type HttpResult<T> = std::result::Result<T, GenericError>;

static NOTFOUND: &[u8] = b"Not Found";

async fn api_post_response() -> HttpResult<Response<Body>> {
    let data = json!({
        "result": "ok",
    });
    let response = Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, "application/json")
        .body(Body::from(data.to_string()))?;
    Ok(response)
}

async fn process_request(
    config: Config,
    req: Request<Body>,
    mut enigo: Enigo,
    ) -> HttpResult<Response<Body>> {
    match req.method() {
        &Method::POST => {
            for m in &config.mappings {
                if req.uri().path() == m.path {
                    m.execute(&mut enigo);
                    return api_post_response().await;
                }
            }
            Ok(Response::builder()
               .status(StatusCode::NOT_FOUND)
               .body(NOTFOUND.into())
               .unwrap())
        },
        _ => {
            // Return 404 not found response.
            Ok(Response::builder()
               .status(StatusCode::NOT_FOUND)
               .body(NOTFOUND.into())
               .unwrap())
        }
    }
}

#[tokio::main]
async fn main() -> HttpResult<()> {
    pretty_env_logger::init();

    let matches = App::new("megaput")
        .version("0.1.0")
        .author("phunx")
        .about("Web service emulating mouse and keyboard inputs")
        .arg(
            Arg::with_name("file")
            .index(1)
            .required(true)
            .help("CSV file containing the mappings")
            )
        .arg(
            Arg::with_name("listen")
            .help("Address to listen on")
            .takes_value(true)
            .short("l")
            .long("listen")
            )
        .get_matches();

    let config = Config::new(String::from(matches.value_of("file").unwrap())).unwrap_or_else(|err| {
        println!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    let addr = match matches.value_of("listen") {
        Some(addr) => addr,
        _ => "127.0.0.1:1337",
    }.parse().unwrap();

    let new_service = make_service_fn(move |_| {
        // Move a clone of `client` into the `service_fn`.
        let config = config.clone();
        async {
            Ok::<_, GenericError>(service_fn(move |req| {
                // Clone again to ensure that client outlives this closure.
                process_request(config.to_owned(), req, Enigo::new())
            }))
        }
    });

    let server = Server::bind(&addr).serve(new_service);

    println!("Listening on http://{}", addr);

    server.await?;

    Ok(())
}

/// An error that can occur when parsing DSL
#[derive(Debug, PartialEq, Eq)]
enum ParseError {
    /// When a tag doesn't exist.
    /// Example: {+TEST}{-TEST}
    ///            ^^^^   ^^^^
    UnknownTag(String),

    /// When a { is encountered inside a {TAG}.
    /// Example: {+HELLO{WORLD}
    ///                 ^
    UnexpectedOpen,

    /// When a { is never matched with a }.
    /// Example: {+SHIFT}Hello{-SHIFT
    ///                              ^
    UnmatchedOpen,

    /// Opposite of UnmatchedOpen.
    /// Example: +SHIFT}Hello{-SHIFT}
    ///         ^
    UnmatchedClose,

    InvalidPosArguments,
}
impl Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            ParseError::UnknownTag(_) => "Unknown tag",
            ParseError::UnexpectedOpen => "Unescaped open bracket ({) found inside tag name",
            ParseError::UnmatchedOpen => "Unmatched open bracket ({). No matching close (})",
            ParseError::UnmatchedClose => "Unmatched close bracket (}). No previous open ({)",
            ParseError::InvalidPosArguments => "Invalid position arguments for mouse movement."
        }
    }
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_string())
    }
}

/// Evaluate the DSL. This tokenizes the input and presses the keys.
fn eval<K>(enigo: &mut K, input: &str) -> Result<(), ParseError>
where
K: KeyboardControllable + MouseControllable,
{
    for token in tokenize(input)? {
        match token {
            Token::Sequence(buffer) => {
                for key in buffer.chars() {
                    enigo.key_click(Key::Layout(key));
                }
            }
            Token::Unicode(buffer) => enigo.key_sequence(&buffer),
            Token::KeyUp(key) => enigo.key_up(key),
            Token::KeyDown(key) => enigo.key_down(key),
            Token::MouseUp(mb) => enigo.mouse_up(mb),
            Token::MouseDown(mb) => enigo.mouse_down(mb),
            Token::MouseClick(mb) => enigo.mouse_click(mb),
            Token::MouseTo(x, y) => enigo.mouse_move_to(x, y),
            Token::MouseRel(x, y) => enigo.mouse_move_relative(x, y),
        }
    }
    Ok(())
}

#[derive(Debug, PartialEq)]
enum Token {
    Sequence(String),
    Unicode(String),
    KeyUp(Key),
    KeyDown(Key),
    MouseUp(MouseButton),
    MouseDown(MouseButton),
    MouseClick(MouseButton),
    MouseTo(i32, i32),
    MouseRel(i32, i32),
}

fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut unicode = false;

    let mut tokens = Vec::new();
    let mut buffer = String::new();
    let mut iter = input.chars().peekable();

    fn flush(tokens: &mut Vec<Token>, buffer: String, unicode: bool) {
        if !buffer.is_empty() {
            if unicode {
                tokens.push(Token::Unicode(buffer));
            } else {
                tokens.push(Token::Sequence(buffer));
            }
        }
    }

    while let Some(c) = iter.next() {
        if c == '{' {
            match iter.next() {
                Some('{') => buffer.push('{'),
                Some(mut c) => {
                    flush(&mut tokens, buffer, unicode);
                    buffer = String::new();

                    let mut tag = String::new();
                    let mut tag_arg = String::new();
                    let mut is_tag = true;
                    loop {
                        if is_tag {
                            tag.push(c);
                        } else {
                            tag_arg.push(c);
                        }
                        match iter.next() {
                            Some('{') => match iter.peek() {
                                Some(&'{') => {
                                    iter.next();
                                    c = '{'
                                }
                                _ => return Err(ParseError::UnexpectedOpen),
                            },
                            Some('}') => {
                                is_tag = true;
                                match iter.peek() {
                                    Some(&'}') => {
                                        iter.next();
                                        c = '}'
                                    }
                                    _ => break,
                                }
                            },
                            Some(':') => {
                                is_tag = false;
                                c = ' '
                            },
                            Some(new) => c = new,
                            None => return Err(ParseError::UnmatchedOpen),
                        }
                    }
                    match &*tag {
                        "+UNICODE" => unicode = true,
                        "-UNICODE" => unicode = false,
                        "+SHIFT" => tokens.push(Token::KeyDown(Key::Shift)),
                        "-SHIFT" => tokens.push(Token::KeyUp(Key::Shift)),
                        "+CTRL" => tokens.push(Token::KeyDown(Key::Control)),
                        "-CTRL" => tokens.push(Token::KeyUp(Key::Control)),
                        "+META" => tokens.push(Token::KeyDown(Key::Meta)),
                        "-META" => tokens.push(Token::KeyUp(Key::Meta)),
                        "+ALT" => tokens.push(Token::KeyDown(Key::Alt)),
                        "-ALT" => tokens.push(Token::KeyUp(Key::Alt)),
                        "+ML" => tokens.push(Token::MouseDown(MouseButton::Left)),
                        "-ML" => tokens.push(Token::MouseUp(MouseButton::Left)),
                        "ML" => tokens.push(Token::MouseClick(MouseButton::Left)),
                        "+MM" => tokens.push(Token::MouseDown(MouseButton::Middle)),
                        "-MM" => tokens.push(Token::MouseUp(MouseButton::Middle)),
                        "MM" => tokens.push(Token::MouseClick(MouseButton::Middle)),
                        "+MR" => tokens.push(Token::MouseDown(MouseButton::Right)),
                        "-MR" => tokens.push(Token::MouseUp(MouseButton::Right)),
                        "MR" => tokens.push(Token::MouseClick(MouseButton::Right)),
                        "MTO" => {
                            let args: Vec<i32> = tag_arg
                                .trim()
                                .split(",")
                                .map(|x| x.parse::<i32>())
                                .filter_map(|x| x.ok())
                                .collect();
                            if args.len() < 2 {
                                return Err(ParseError::InvalidPosArguments);
                            }

                            tokens.push(Token::MouseTo(args[0], args[1]))
                        },
                        "MREL" => {
                            let args: Vec<i32> = tag_arg
                                .trim()
                                .split(",")
                                .map(|x| x.parse::<i32>())
                                .filter_map(|x| x.ok())
                                .collect();
                            if args.len() < 2 {
                                return Err(ParseError::InvalidPosArguments);
                            }

                            tokens.push(Token::MouseRel(args[0], args[1]))
                        }
                        _ => return Err(ParseError::UnknownTag(tag)),
                    }
                }
                None => return Err(ParseError::UnmatchedOpen),
            }
        } else if c == '}' {
            match iter.next() {
                Some('}') => buffer.push('}'),
                _ => return Err(ParseError::UnmatchedClose),
            }
        } else {
            buffer.push(c);
        }
    }

    flush(&mut tokens, buffer, unicode);

    Ok(tokens)
}

#[derive(Clone, Debug, Deserialize)]
struct CommandMapping {
    path: String,
    command: String,
}

impl CommandMapping {
    fn execute(&self, enigo: &mut Enigo) {
        println!("{}", self.command);
        eval(enigo, &self.command).expect("Could not parse sequence");
    }
}

#[derive(Clone, Debug)]
struct Config {
    filename: String,
    mappings: Vec<CommandMapping>,
}

impl Config {
    fn new(filename: String) -> Result<Config, Box<dyn Error>> {
        let mut rdr = csv::Reader::from_path(&filename)?;
        let mappings = rdr.deserialize().filter_map(|x| x.ok()).collect();

        println!("{:#?}", mappings);
        Ok(Config { filename, mappings })
    }
}
