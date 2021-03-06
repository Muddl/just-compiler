use std::fmt;

/// Keywords of `rust-lexer`
#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    If, Then, Else,
    Do, While, End,
    Repeat, Until,
    Print,
    Function,
    Id,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Keyword::If => write!(f, "IF_KEY"),
            Keyword::Then => write!(f, "THEN_KEY"),
            Keyword::Else => write!(f, "ELSE_KEY"),
            Keyword::Do => write!(f, "DO_KEY"),
            Keyword::While => write!(f, "WHILE_KEY"),
            Keyword::End => write!(f, "END_KEY"),
            Keyword::Repeat => write!(f, "REPEAT_KEY"),
            Keyword::Until => write!(f, "UNTIL_KEY"),
            Keyword::Print => write!(f, "PRINT_KEY"),
            Keyword::Function => write!(f, "FUNCTION_KEY"),
            Keyword::Id => write!(f, "ID_KEY"),
        }
    }
}

/// Operators of `rust-lexer`
#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Assign, Eq, NotEq, 
    LessEq, Less,
    GreatEq, Great,
    Add, Sub, Mul, Div,
    LOr, LAnd, LNot
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operator::Assign => write!(f, "ASSIGN_OP"),
            Operator::Eq => write!(f, "EQUALS_OP"),
            Operator::NotEq => write!(f, "NOT_EQUALS_OP"),
            Operator::LessEq => write!(f, "LESS_EQUALS_OP"),
            Operator::Less => write!(f, "LESS_OP"),
            Operator::GreatEq => write!(f, "GREAT_EQUALS_OP"),
            Operator::Great => write!(f, "GREAT_OP"),
            Operator::Add => write!(f, "ADD_OP"),
            Operator::Sub => write!(f, "SUB_OP"),
            Operator::Mul => write!(f, "MUL_OP"),
            Operator::Div => write!(f, "DIV_OP"),
            Operator::LOr => write!(f, "LOr"),
            Operator::LAnd => write!(f, "LAnd"),
            Operator::LNot => write!(f, "LNot"),
        }
    }
}

/// Operators of `rust-lexer`
#[derive(Clone, Debug, PartialEq)]
pub enum Delimiter {
    ParenPair, OpenParen, CloseParen,
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Delimiter::ParenPair => write!(f, "PARENTHESES_PAIR"),
            Delimiter::OpenParen => write!(f, "PARENTHESES_OPEN"),
            Delimiter::CloseParen => write!(f, "PARENTHESES_CLOSE"),
        }
    }
}

/// Token generated by `Lexer`.
#[derive(Debug, PartialEq)]
pub enum Token {
    /// End of file
    End,
    /// Identifier
    Id(String),
    /// Integer Literal
    Int(i32),
    /// Keyword
    Key(Keyword),
    /// Operator
    Op(Operator),
    /// Delimiter
    Delim(Delimiter),
    /// Other characters
    Other(char),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            Token::End => write!(f, "EOL_CHAR"),
            Token::Id(String) => write!(f, "ID"),
            Token::Int(i32) => write!(f, "INTEGER"),
            Token::Key(Keyword) => write!(f, "{}", Keyword),
            Token::Op(Operator) => write!(f, "{}", Operator),
            Token::Delim(Delimiter) => write!(f, "{}", Delimiter),
            Token::Other(char) => write!(f, "OTHER"),
        }
    }
}