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
pub struct FunctionDefinition {
  pub name: Identifier,
  pub parameters: Vec<Identifier>,
  pub returns: Vec<Identifier>,
  pub block: Block,
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Assignment {
  pub identifiers: Vec<Identifier>,
  pub expression: Expression,
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub enum Expression {
  Literal(Literal),
  Identifier(Identifier),
  FunctionCall(FunctionCall),
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct If {
  pub expression: Expression,
  pub block: Block,
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Case {
  pub literal: Literal,
  pub block: Block
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct Switch {
  pub expression: Expression,
  pub cases: Vec<Case>,
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub struct ForLoop {
  pub pre: Block,
  pub condition: Expression,
  pub post: Block,
  pub body: Block,
}

#[derive(Hash,Clone,PartialEq,Debug)]
pub enum Statement {
  Block(Block),
  FunctionDefinition(FunctionDefinition),
  VariableDeclaration(Vec<Identifier>, Option<Expression>),
  Assignment(Assignment),
  Expression(Expression),
  If(If),
  Switch(Switch),
  ForLoop(ForLoop),
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

impl fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "function {}(", self.name));
        for (i, identifier) in self.parameters.iter().enumerate() {
            try!(write!(f, "{}", identifier));
            if i < self.parameters.len() - 1 {
                try!(write!(f, ", "));
            }
        }
        try!(write!(f, ")"));
        if self.returns.len() > 0 {
            try!(write!(f, " -> "));
            for (i, identifier) in self.returns.iter().enumerate() {
                try!(write!(f, "{}", identifier));
                if i < self.returns.len() - 1 {
                    try!(write!(f, ", "));
                }
            }
        }
        write!(f, " {}", self.block)
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.identifiers.len() == 0 {
            panic!("Assignment must have identifiers")
        }
        for (i, identifier) in self.identifiers.iter().enumerate() {
            try!(write!(f, "{}", identifier));
            if i < self.identifiers.len() - 1 {
                try!(write!(f, ", "));
            }
        }
        write!(f, " := {}", self.expression)
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

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {}", self.expression, self.block)
    }
}

impl fmt::Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.literal.literal.len() == 0 {
            write!(f, "default: {}", self.block)
        } else {
            write!(f, "case {}: {}", self.literal, self.block)
        }
    }
}

impl fmt::Display for Switch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "switch {} ", self.expression));
        for case in &self.cases {
            try!(write!(f, "{} ", case));
        }
        write!(f, "")
    }
}

impl fmt::Display for ForLoop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "for {} {} {} {}", self.pre, self.condition, self.post, self.body)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Block(ref block) => write!(f, "{}", block),
            Statement::FunctionDefinition(ref function) => write!(f, "{}", function),
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
            Statement::Assignment(ref assignment) => write!(f, "{}", assignment),
            Statement::Expression(ref expression) => write!(f, "{}", expression),
            Statement::If(ref if_statement) => write!(f, "{}", if_statement),
            Statement::Switch(ref switch) => write!(f, "{}", switch),
            Statement::ForLoop(ref forloop) => write!(f, "{}", forloop),
            Statement::Break => write!(f, "break"),
            Statement::Continue => write!(f, "continue"),
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
        let tmp = If{ expression: exp, block: block };
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
        let tmp = Assignment{ identifiers: vec!{name}, expression: exp };
        assert_eq!(tmp.to_string(), "a := 1");
    }

    #[test]
    fn assignment_multi() {
        let lit = Literal{ literal: "1".to_string() };
        let exp = Expression::Literal(lit);
        let name = Identifier{ identifier: "a".to_string() };
        let tmp = Assignment{ identifiers: vec!{name.clone(), name.clone(), name.clone()}, expression: exp };
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
        let tmp = FunctionDefinition{ name: name, parameters: vec![], returns: vec![], block: empty_block };
        assert_eq!(tmp.to_string(), "function name() { }");
    }

    #[test]
    fn functiondefinition_single_arg() {
        let empty_block = Block{ statements: vec![] };
        let name = Identifier{ identifier: "name".to_string() };
        let tmp = FunctionDefinition{ name: name.clone(), parameters: vec!{name.clone()}, returns: vec![], block: empty_block };
        assert_eq!(tmp.to_string(), "function name(name) { }");
    }

    #[test]
    fn functiondefinition_single_ret() {
        let empty_block = Block{ statements: vec![] };
        let name = Identifier{ identifier: "name".to_string() };
        let tmp = FunctionDefinition{ name: name.clone(), parameters: vec![], returns: vec!{name.clone()}, block: empty_block };
        assert_eq!(tmp.to_string(), "function name() -> name { }");
    }

    #[test]
    fn functiondefinition_multi() {
        let empty_block = Block{ statements: vec![] };
        let name = Identifier{ identifier: "name".to_string() };
        let tmp = FunctionDefinition{ name: name.clone(), parameters: vec!{name.clone(), name.clone()}, returns: vec!{name.clone(), name.clone()}, block: empty_block };
        assert_eq!(tmp.to_string(), "function name(name, name) -> name, name { }");
    }

    #[test]
    fn case() {
        let block = Block{ statements: vec![] };
        let lit = Literal{ literal: "literal".to_string() };
        let tmp = Case{ literal: lit, block: block };
        assert_eq!(tmp.to_string(), "case literal: { }");
    }

    #[test]
    fn case_default() {
        let block = Block{ statements: vec![] };
        let lit = Literal{ literal: "".to_string() };
        let tmp = Case{ literal: lit, block: block };
        assert_eq!(tmp.to_string(), "default: { }");
    }

    #[test]
    fn switch() {
        let block = Block{ statements: vec![] };
        let emptylit = Literal{ literal: "".to_string() };
        let defaultcase = Case{ literal: emptylit, block: block.clone() };
        let lit = Literal{ literal: "1".to_string() };
        let case = Case{ literal: lit, block: block.clone() };
        let exp = Expression::Literal(Literal{ literal: "3".to_string() });
        let tmp = Switch{ expression: exp, cases: vec!{case, defaultcase}};
        assert_eq!(tmp.to_string(), "switch 3 case 1: { } default: { } ");
    }

    #[test]
    fn forloop() {
        let block = Block{ statements: vec![] };
        let exp = Expression::Literal(Literal{ literal: "1".to_string() });
        let tmp = ForLoop{ pre: block.clone(), condition: exp, post: block.clone(), body: block.clone() };
        assert_eq!(tmp.to_string(), "for { } 1 { } { }");
    }
}
