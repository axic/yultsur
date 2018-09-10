use std::fmt;

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Block {
  pub statements: Vec<Statement>
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Literal {
  pub literal: String
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Identifier {
  pub identifier: String
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct FunctionCall {
  pub identifier: Identifier,
  pub arguments: Vec<Expression>,
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub enum Expression {
  Literal(Literal),
  Identifier(Identifier),
  FunctionCall(FunctionCall),
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Case {
  pub literal: Literal,
  pub block: Block
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub enum Statement {
  Block(Block),
  FunctionDefinition(Identifier, Vec<Identifier>, Vec<Identifier>, Block),
  VariableDeclaration(Vec<Identifier>, Option<Expression>),
  Assignment(Vec<Identifier>, Expression),
  Expression(Expression),
  If(Expression, Block),
  Switch(Expression, Vec<Case>),
  SwitchDefault(Expression, Vec<Case>, Block),
  ForLoop(Block, Expression, Block, Block),
  Break,
  Continue,
}

impl Identifier {
    pub fn new(identifier: &str) -> Self {
        Identifier {
            identifier: identifier.to_string()
        }
    }
}

impl Literal {
    pub fn new(literal: &str) -> Self {
        Literal {
            literal: literal.to_string()
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}(", self.identifier));
        for (i, argument) in self.arguments.iter().enumerate() {
            try!(write!(f, "{}", argument));
            if i < self.arguments.len() - 1 {
                try!(write!(f, ","));
            }
        }
        write!(f, ")")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Literal(ref literal) => write!(f, "{}", literal),
            Expression::Identifier(ref identifier) => write!(f, "{}", identifier),
            Expression::FunctionCall(ref functioncall) => write!(f, "{}", functioncall),
        }
    }
}

impl fmt::Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.literal.literal.len() == 0 {
            write!(f, "default:")
        } else {
            write!(f, "case {}:", self.literal)
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Block(ref block) => write!(f, "{}", block),
            Statement::FunctionDefinition(ref identifier, ref parameters, ref returns, ref block) => {
                try!(write!(f, "function {}(", identifier));
                for (i, identifier) in parameters.iter().enumerate() {
                    try!(write!(f, "{}", identifier));
                    if i < parameters.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                try!(write!(f, ")"));
                if returns.len() > 0 {
                    try!(write!(f, " -> "));
                    for (i, identifier) in returns.iter().enumerate() {
                        try!(write!(f, "{}", identifier));
                        if i < returns.len() - 1 {
                            try!(write!(f, ", "));
                        }
                    }
                }
                write!(f, " {}", block)
            },
            Statement::VariableDeclaration(ref identifiers, ref expression) => {
                if identifiers.len() == 0 {
                  panic!("VariableDeclaration must have identifiers")
                }
                try!(write!(f, "let "));
                for (i, identifier) in identifiers.iter().enumerate() {
                    try!(write!(f, "{}", identifier));
                    if i < identifiers.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                if let Some(expression) = expression {
                    write!(f, " := {}", expression)
                } else {
                    write!(f, "")
                }
            },
            Statement::Assignment(ref identifiers, ref expression) => {
                if identifiers.len() == 0 {
                    panic!("Assignment must have identifiers")
                }
                for (i, identifier) in identifiers.iter().enumerate() {
                    try!(write!(f, "{}", identifier));
                    if i < identifiers.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                write!(f, " := {}", expression)
            },
            Statement::Expression(ref expression) => write!(f, "{}", expression),
            Statement::If(ref expression, ref block) => write!(f, "if {} {}", expression, block),
            Statement::Break => write!(f, "break"),
            Statement::Continue => write!(f, "continue"),
            _ => panic!()
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{{"));
        for (_, statement) in self.statements.iter().enumerate() {
            try!(write!(f, " {}", statement));
        }
        write!(f, " }}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal() {
        let tmp = Literal{ literal: "testliteral".to_string() };
        assert_eq!(tmp.to_string(), "testliteral");
    }

    #[test]
    fn identifier() {
        let tmp = Identifier{ identifier: "testidentifier".to_string() };
        assert_eq!(tmp.to_string(), "testidentifier");
    }

    #[test]
    fn functioncall() {
        let name = Identifier{ identifier: "test".to_string() };
        let lit = Literal{ literal: "literal".to_string() };
        let args = vec!{Expression::Identifier(name.clone()), Expression::Literal(lit.clone())};
        let tmp = FunctionCall{ identifier: name, arguments: args };
        assert_eq!(tmp.to_string(), "test(test,literal)");
    }

    #[test]
    fn if_statement() {
        let lit = Literal{ literal: "literal".to_string() };
        let exp = Expression::Literal(lit);
        let block = Block{ statements: vec![] };
        let tmp = Statement::If(exp, block);
        assert_eq!(tmp.to_string(), "if literal { }");
    }

    #[test]
    fn block_empty() {
        let block = Block{ statements: vec![] };
        assert_eq!(block.to_string(), "{ }");
    }

    #[test]
    fn block_nested() {
        let empty_block = Block{ statements: vec![] };
        let block = Block{ statements: vec!{Statement::Block(empty_block)} };
        assert_eq!(block.to_string(), "{ { } }");
    }

    #[test]
    fn block_literal() {
        let lit = Literal{ literal: "literal".to_string() };
        let exp = Expression::Literal(lit);
        let block = Block{ statements: vec!{Statement::Expression(exp)} };
        assert_eq!(block.to_string(), "{ literal }");
    }

    #[test]
    fn assignment_single() {
        let lit = Literal{ literal: "1".to_string() };
        let exp = Expression::Literal(lit);
        let name = Identifier{ identifier: "a".to_string() };
        let tmp = Statement::Assignment(vec!{name}, exp);
        assert_eq!(tmp.to_string(), "a := 1");
    }

    #[test]
    fn assignment_multi() {
        let lit = Literal{ literal: "1".to_string() };
        let exp = Expression::Literal(lit);
        let name = Identifier{ identifier: "a".to_string() };
        let tmp = Statement::Assignment(vec!{name.clone(), name.clone(), name.clone()}, exp);
        assert_eq!(tmp.to_string(), "a, a, a := 1");
    }

    #[test]
    fn variabledeclaration_empty() {
        let name = Identifier{ identifier: "a".to_string() };
        let tmp = Statement::VariableDeclaration(vec!{name}, None);
        assert_eq!(tmp.to_string(), "let a");
    }

    #[test]
    fn variabledeclaration_single() {
        let lit = Literal{ literal: "1".to_string() };
        let exp = Expression::Literal(lit);
        let name = Identifier{ identifier: "a".to_string() };
        let tmp = Statement::VariableDeclaration(vec!{name}, Some(exp));
        assert_eq!(tmp.to_string(), "let a := 1");
    }

    #[test]
    fn variabledeclaration_multi() {
        let lit = Literal{ literal: "1".to_string() };
        let exp = Expression::Literal(lit);
        let name = Identifier{ identifier: "a".to_string() };
        let tmp = Statement::VariableDeclaration(vec!{name.clone(), name.clone(), name.clone()}, Some(exp));
        assert_eq!(tmp.to_string(), "let a, a, a := 1");
    }

    #[test]
    fn functiondefinition_basic() {
        let empty_block = Block{ statements: vec![] };
        let name = Identifier{ identifier: "name".to_string() };
        let tmp = Statement::FunctionDefinition(name, vec![], vec![], empty_block);
        assert_eq!(tmp.to_string(), "function name() { }");
    }


    #[test]
    fn functiondefinition_single_arg() {
        let empty_block = Block{ statements: vec![] };
        let name = Identifier{ identifier: "name".to_string() };
        let tmp = Statement::FunctionDefinition(name.clone(), vec!{name.clone()}, vec![], empty_block);
        assert_eq!(tmp.to_string(), "function name(name) { }");
    }

    #[test]
    fn functiondefinition_single_ret() {
        let empty_block = Block{ statements: vec![] };
        let name = Identifier{ identifier: "name".to_string() };
        let tmp = Statement::FunctionDefinition(name.clone(), vec![], vec!{name.clone()}, empty_block);
        assert_eq!(tmp.to_string(), "function name() -> name { }");
    }

    #[test]
    fn functiondefinition_multi() {
        let empty_block = Block{ statements: vec![] };
        let name = Identifier{ identifier: "name".to_string() };
        let tmp = Statement::FunctionDefinition(name.clone(), vec!{name.clone(), name.clone()}, vec!{name.clone(), name.clone()}, empty_block);
        assert_eq!(tmp.to_string(), "function name(name, name) -> name, name { }");
    }

    #[test]
    fn case() {
        let block = Block{ statements: vec![] };
        let lit = Literal{ literal: "literal".to_string() };
        let tmp = Case{ literal: lit, block: block };
        assert_eq!(tmp.to_string(), "case literal:");
    }

    #[test]
    fn case_default() {
        let block = Block{ statements: vec![] };
        let lit = Literal{ literal: "".to_string() };
        let tmp = Case{ literal: lit, block: block };
        assert_eq!(tmp.to_string(), "default:");
    }
}
