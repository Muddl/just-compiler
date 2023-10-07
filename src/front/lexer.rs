use crate::define::{Keyword, Operator, Delimiter, Token};
use phf::{phf_map};
use std::io::Read;

/// Lexer for `Julia` language used in `rust-lexer`
pub struct Lexer<T: Read> {
    reader: T,
    last_char: Option<char>,
}

/// `Result` for token handlers of `Lexer`
pub type Result = std::result::Result<Token, String>;

impl<T: Read> Lexer<T> {
    /// Creates a new `Lexer` object from the specific `Read` object.
    pub fn new(reader: T) -> Self {
        Self {
            reader,
            last_char: Some(' '),
        }
    }

    /// Gets the next token from file
    pub fn next_token(&mut self) -> Result {
        // skip spaces
        while self.last_char.map_or(false, |c| c.is_whitespace()) {
            self.next_char()?;
        }
        // check last character
        if let Some(c) = self.last_char {
            if c == '/' {
                // skip comments
                self.handle_comment()
            } else if c.is_numeric() {
                // integer literal
                self.handle_integer()
            } else if c.is_alphanumeric() || c == '_' {
                // id or keyword
                self.handle_id()
            } else if is_operator_char(c) {
                // operator
                self.handle_operator()
            } else if is_delimiter_char(c) {
                // delimiter
                self.handle_delimiter()
            } else {
                // other characters
                self.next_char()?;
                Ok(Token::Other(c))
            }
        } else {
            // may be EOF, or other file errors
            Ok(Token::End)
        }
    }

    /// Reads a character from file.
    fn next_char(&mut self) -> std::result::Result<(), String> {
        // NOTE: UTF-8 characters will not be handled here.
        let mut single_char = [0];
        self.last_char = (self
            .reader
            .read(&mut single_char)
            .map_err(|err| format!("{}", err))?
            != 0)
            .then(|| single_char[0] as char);
        Ok(())
    }

    /// Handles identifiers or keywords.
    fn handle_id(&mut self) -> Result {
        // read to string
        let mut id = String::new();
        while self
            .last_char
            .map_or(false, |c| c.is_alphanumeric() || c == '_')
        {
            id.push(self.last_char.unwrap());
            self.next_char()?;
        }
        // check if string is keyword
        if let Some(keyword) = parse_keyword(&id) {
            Ok(Token::Key(keyword))
        } else {
            Ok(Token::Id(id))
        }
    }

    /// Handles integer literals.
    fn handle_integer(&mut self) -> Result {
        // read to string
        let mut num = String::new();
        while self.last_char.map_or(false, |c| c.is_numeric()) {
            num.push(self.last_char.unwrap());
            self.next_char()?;
        }
        // convert to integer
        num
            .parse::<i32>()
            .map(|i| Token::Int(i))
            .map_err(|_| String::from("invalid integer literal"))
    }

    /// Handles operators.
    fn handle_operator(&mut self) -> Result {
        // read to string
        let mut op = String::new();
        while self.last_char.map_or(false, |c| is_operator_char(c)) {
            op.push(self.last_char.unwrap());
            self.next_char()?;
        }
        // check if is a valid operator
        parse_operator(&op)
            .map(|op| Token::Op(op))
            .ok_or(String::from("invalid operator"))
    }

    /// Handles comment.
    fn handle_comment(&mut self) -> Result {
        // skip the current line
        while self.last_char.map_or(false, |c| c != '\r' && c != '\n') {
            self.next_char()?;
        }
        // return the next token
        self.next_token()
    }

    /// Handles delimiters.
    fn handle_delimiter(&mut self) -> Result {
        // read to string
        let mut de = String::new();
        while self.last_char.map_or(false, |c| is_delimiter_char(c)) {
            de.push(self.last_char.unwrap());
            self.next_char()?;
        }
        // check if is a valid operator
        parse_delimiter(&de)
            .map(|de| Token::Delim(de))
            .ok_or(String::from("invalid delimiter"))
    }
}

/// Checks whether the specific character may appear in the operator.
fn is_operator_char(c: char) -> bool {
    "+-*/%<=!&|:".contains(c)
}

/// Checks whether the specific character may appear in the delimiter.
fn is_delimiter_char(c: char) -> bool {
    ")(".contains(c)
}

/// Parses keyword from the specific string.
fn parse_keyword(s: &str) -> Option<Keyword> {
    static KEYWORDS: phf::Map<&'static str, Keyword> = phf_map! {
        "if" => Keyword::If,
        "then" => Keyword::Then,
        "else" => Keyword::Else,
        "end" => Keyword::End,
        "while" => Keyword::While,
        "do" => Keyword::Do,
        "repeat" => Keyword::Repeat,
        "until" => Keyword::Until,
        "print" => Keyword::Print,
        "function" => Keyword::Function,
        "id" => Keyword::Id,
    };
    KEYWORDS.get(s).cloned()
}

/// Parses operator from the specific string.
fn parse_operator(s: &str) -> Option<Operator> {
    static OPERATORS: phf::Map<&'static str, Operator> = phf_map! {
        "+" => Operator::Add,
        "-" => Operator::Sub,
        "*" => Operator::Mul,
        "/" => Operator::Div,
        "<" => Operator::Less,
        "<=" => Operator::LessEq,
        ">" => Operator::Great,
        ">=" => Operator::GreatEq,
        "==" => Operator::Eq,
        "!=" => Operator::NotEq,
        "=" => Operator::Assign,
        "&&" => Operator::LAnd,
        "||" => Operator::LOr,
        "!" => Operator::LNot
    };
    OPERATORS.get(s).cloned()
}

/// Parses delimiter from the specific string.
fn parse_delimiter(s: &str) -> Option<Delimiter> {
    static DELIMITERS: phf::Map<&'static str, Delimiter> = phf_map! {
        "(" => Delimiter::OpenParen,
        ")" => Delimiter::CloseParen,
        "()" => Delimiter::ParenPair,
    };
    DELIMITERS.get(s).cloned()
}