use std::fmt;

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Block {
  statements: Vec<Statement>
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Literal {
  literal: String
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Identifier {
  identifier: String
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub enum Expression {
  Literal(Literal),
  Identifier(Identifier),
  FunctionCall(Identifier, Vec<Expression>),
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Case {
  literal: Literal,
  block: Block
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub enum Statement {
  Block(Block),
  FunctionDefinition(Identifier, Vec<Identifier>, Vec<Identifier>),
  VariableDeclaration(Vec<Identifier>, Expression),
  Assignment(Vec<Identifier>, Expression),
  Expression(Expression),
  If(Expression, Block),
  Switch(Expression, Vec<Case>),
  SwitchDefault(Expression, Vec<Case>, Block),
  ForLoop(Block, Expression, Block, Block),
  Break,
  Continue,
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Literal(ref literal) => write!(f, "{}", literal),
            Expression::Identifier(ref identifier) => write!(f, "{}", identifier),
            Expression::FunctionCall(ref identifier, ref expressions) => {
                write!(f, "{}(", identifier);
                for (i, expression) in expressions.iter().enumerate() {
                    write!(f, "{}", expression);
                    if i < expressions.len() - 1 {
                        write!(f, ",");
                    }
                }
                write!(f, ")")
            }
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
        write!(f, "{{");
        for (i, statement) in self.statements.iter().enumerate() {
            write!(f, " {}", statement);
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
    fn expression() {
        let name = Identifier{ identifier: "test".to_string() };
        let lit = Literal{ literal: "literal".to_string() };
        let args = vec!{Expression::Identifier(name.clone()), Expression::Literal(lit.clone())};
        let tmp = Expression::FunctionCall(name, args);
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
