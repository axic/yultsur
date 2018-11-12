use std::fmt;

#[derive(Hash, Clone, PartialEq, Debug)]
pub enum Type {
    Bool,
    Uint256,
    Uint128,
    Uint64,
    Uint32,
    Uint8,
    Int256,
    Int128,
    Int64,
    Int32,
    Int8,
    Custom(Identifier),
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Literal {
    pub literal: String,
    pub yultype: Option<Type>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Identifier(pub String);

impl<S: Into<String>> From<S> for Identifier {
    fn from(string: S) -> Self {
        Identifier(string.into())
    }
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct TypedIdentifier {
    pub identifier: Identifier,
    pub yultype: Type,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct FunctionCall {
    pub identifier: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<TypedIdentifier>,
    pub returns: Vec<TypedIdentifier>,
    pub block: Block,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct VariableDeclaration {
    pub identifiers: Vec<TypedIdentifier>,
    pub expression: Option<Expression>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Assignment {
    pub identifiers: Vec<Identifier>,
    pub expression: Expression,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    FunctionCall(FunctionCall),
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct If {
    pub expression: Expression,
    pub block: Block,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Case {
    pub literal: Option<Literal>,
    pub block: Block,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Switch {
    pub expression: Expression,
    pub cases: Vec<Case>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct ForLoop {
    pub pre: Block,
    pub condition: Expression,
    pub post: Block,
    pub body: Block,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub enum Statement {
    Block(Block),
    FunctionDefinition(FunctionDefinition),
    VariableDeclaration(VariableDeclaration),
    Assignment(Assignment),
    Expression(Expression),
    If(If),
    Switch(Switch),
    ForLoop(ForLoop),
    Break,
    Continue,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Bool => write!(f, "bool"),
            Type::Uint256 => write!(f, "u256"),
            Type::Uint128 => write!(f, "u128"),
            Type::Uint64 => write!(f, "u64"),
            Type::Uint32 => write!(f, "u32"),
            Type::Uint8 => write!(f, "u8"),
            Type::Int256 => write!(f, "i256"),
            Type::Int128 => write!(f, "i128"),
            Type::Int64 => write!(f, "i64"),
            Type::Int32 => write!(f, "i32"),
            Type::Int8 => write!(f, "i8"),
            Type::Custom(ref name) => write!(f, "{}", name),
        }
    }
}

impl Identifier {
    pub fn new(identifier: &str) -> Self {
        identifier.into()
    }
}

impl Literal {
    pub fn new(literal: &str) -> Self {
        Literal {
            literal: literal.to_string(),
            yultype: None,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}", self.literal));
        if let Some(yultype) = &self.yultype {
            write!(f, ":{}", yultype)
        } else {
            write!(f, "")
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for TypedIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.identifier, self.yultype)
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}(", self.identifier));
        try!(write!(
            f,
            "{}",
            self.arguments
                .iter()
                .map(|argument| format!("{}", argument))
                .collect::<Vec<_>>()
                .join(", ")
        ));
        write!(f, ")")
    }
}

impl fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "function {}(", self.name));
        try!(write!(
            f,
            "{}",
            self.parameters
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        ));
        try!(write!(f, ")"));
        if self.returns.len() > 0 {
            try!(write!(f, " -> "));
            try!(write!(
                f,
                "{}",
                self.returns
                    .iter()
                    .map(|identifier| format!("{}", identifier))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
        write!(f, " {}", self.block)
    }
}

impl fmt::Display for VariableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.identifiers.len() == 0 {
            panic!("VariableDeclaration must have identifiers")
        }
        try!(write!(f, "let "));
        try!(write!(
            f,
            "{}",
            self.identifiers
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        ));
        if let Some(expression) = &self.expression {
            write!(f, " := {}", expression)
        } else {
            write!(f, "")
        }
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.identifiers.len() == 0 {
            panic!("Assignment must have identifiers")
        }
        try!(write!(
            f,
            "{}",
            self.identifiers
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        ));
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
        if let Some(literal) = &self.literal {
            // FIXME: should validate this on the new/default trait
            if literal.literal.len() == 0 {
                panic!("Case with literal should not be empty");
            }
            write!(f, "case {} {}", literal, self.block)
        } else {
            write!(f, "default {}", self.block)
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
        write!(
            f,
            "for {} {} {} {}",
            self.pre, self.condition, self.post, self.body
        )
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Block(ref block) => write!(f, "{}", block),
            Statement::FunctionDefinition(ref function) => write!(f, "{}", function),
            Statement::VariableDeclaration(ref variabledeclaration) => {
                write!(f, "{}", variabledeclaration)
            }
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
        assert_eq!(
            Literal {
                literal: "testliteral".to_string(),
                yultype: None,
            }.to_string(),
            "testliteral"
        );
    }

    #[test]
    fn literal_typed() {
        assert_eq!(
            Literal {
                literal: "testliteral".to_string(),
                yultype: Some(Type::Uint256),
            }.to_string(),
            "testliteral:u256"
        );
    }

    #[test]
    fn literal_custom_typed() {
        assert_eq!(
            Literal {
                literal: "testliteral".to_string(),
                yultype: Some(Type::Custom("memptr".into())),
            }.to_string(),
            "testliteral:memptr"
        );
    }

    #[test]
    fn identifier() {
        assert_eq!(
            Identifier::new("testidentifier").to_string(),
            "testidentifier"
        );
    }

    #[test]
    fn typed_identifier() {
        assert_eq!(
            TypedIdentifier {
                identifier: "testidentifier".into(),
                yultype: Type::Uint256,
            }.to_string(),
            "testidentifier:u256"
        );
    }

    #[test]
    fn typed_identifier_custom() {
        assert_eq!(
            TypedIdentifier {
                identifier: "testidentifier".into(),
                yultype: Type::Custom("memptr".into()),
            }.to_string(),
            "testidentifier:memptr"
        );
    }

    #[test]
    fn functioncall() {
        assert_eq!(
            FunctionCall {
                identifier: "test".into(),
                arguments: vec![
                    Expression::Identifier("test".into()),
                    Expression::Literal(Literal {
                        literal: "literal".to_string(),
                        yultype: None,
                    }),
                ],
            }.to_string(),
            "test(test, literal)"
        );
    }

    #[test]
    fn if_statement() {
        assert_eq!(
            If {
                expression: Expression::Literal(Literal {
                    literal: "literal".to_string(),
                    yultype: None,
                }),
                block: Block { statements: vec![] },
            }.to_string(),
            "if literal { }"
        );
    }

    #[test]
    fn block_empty() {
        assert_eq!(Block { statements: vec![] }.to_string(), "{ }");
    }

    #[test]
    fn block_nested() {
        assert_eq!(
            Block {
                statements: vec![Statement::Block(Block { statements: vec![] })],
            }.to_string(),
            "{ { } }"
        );
    }

    #[test]
    fn block_literal() {
        assert_eq!(
            Block {
                statements: vec![Statement::Expression(Expression::Literal(Literal {
                    literal: "literal".to_string(),
                    yultype: None,
                }))],
            }.to_string(),
            "{ literal }"
        );
    }

    #[test]
    fn assignment_single() {
        assert_eq!(
            Assignment {
                identifiers: vec![Identifier::new("a")],
                expression: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                }),
            }.to_string(),
            "a := 1"
        );
    }

    #[test]
    fn assignment_multi() {
        assert_eq!(
            Assignment {
                identifiers: vec![
                    Identifier::new("a"),
                    Identifier::new("b"),
                    Identifier::new("c"),
                ],
                expression: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                }),
            }.to_string(),
            "a, b, c := 1"
        );
    }

    #[test]
    fn variabledeclaration_empty() {
        assert_eq!(
            VariableDeclaration {
                identifiers: vec![TypedIdentifier {
                    identifier: "a".into(),
                    yultype: Type::Uint256,
                }],
                expression: None,
            }.to_string(),
            "let a:u256"
        );
    }

    #[test]
    fn variabledeclaration_single() {
        assert_eq!(
            VariableDeclaration {
                identifiers: vec![TypedIdentifier {
                    identifier: "a".into(),
                    yultype: Type::Uint256,
                }],
                expression: Some(Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                })),
            }.to_string(),
            "let a:u256 := 1"
        );
    }

    #[test]
    fn variabledeclaration_multi() {
        assert_eq!(
            VariableDeclaration {
                identifiers: vec![
                    TypedIdentifier {
                        identifier: "a".into(),
                        yultype: Type::Uint256,
                    },
                    TypedIdentifier {
                        identifier: "b".into(),
                        yultype: Type::Uint64,
                    },
                    TypedIdentifier {
                        identifier: "c".into(),
                        yultype: Type::Uint8,
                    },
                ],
                expression: Some(Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                })),
            }.to_string(),
            "let a:u256, b:u64, c:u8 := 1"
        );
    }

    #[test]
    fn functiondefinition_basic() {
        assert_eq!(
            FunctionDefinition {
                name: "name".into(),
                parameters: vec![],
                returns: vec![],
                block: Block { statements: vec![] },
            }.to_string(),
            "function name() { }"
        );
    }

    #[test]
    fn functiondefinition_single_arg() {
        assert_eq!(
            FunctionDefinition {
                name: "name".into(),
                parameters: vec![TypedIdentifier {
                    identifier: "a".into(),
                    yultype: Type::Uint8,
                }],
                returns: vec![],
                block: Block { statements: vec![] },
            }.to_string(),
            "function name(a:u8) { }"
        );
    }

    #[test]
    fn functiondefinition_single_ret() {
        assert_eq!(
            FunctionDefinition {
                name: "name".into(),
                parameters: vec![],
                returns: vec![TypedIdentifier {
                    identifier: "a".into(),
                    yultype: Type::Uint8,
                }],
                block: Block { statements: vec![] },
            }.to_string(),
            "function name() -> a:u8 { }"
        );
    }

    #[test]
    fn functiondefinition_multi() {
        assert_eq!(
            FunctionDefinition {
                name: "name".into(),
                parameters: vec![
                    TypedIdentifier {
                        identifier: "a".into(),
                        yultype: Type::Uint8,
                    },
                    TypedIdentifier {
                        identifier: "b".into(),
                        yultype: Type::Uint8,
                    },
                ],
                returns: vec![
                    TypedIdentifier {
                        identifier: "c".into(),
                        yultype: Type::Uint8,
                    },
                    TypedIdentifier {
                        identifier: "d".into(),
                        yultype: Type::Uint8,
                    },
                ],
                block: Block { statements: vec![] },
            }.to_string(),
            "function name(a:u8, b:u8) -> c:u8, d:u8 { }"
        );
    }

    #[test]
    fn case() {
        assert_eq!(
            Case {
                literal: Some(Literal {
                    literal: "literal".to_string(),
                    yultype: None,
                }),
                block: Block { statements: vec![] },
            }.to_string(),
            "case literal { }"
        );
    }

    #[test]
    fn case_default() {
        assert_eq!(
            Case {
                literal: None,
                block: Block { statements: vec![] },
            }.to_string(),
            "default { }"
        );
    }

    #[test]
    fn switch() {
        assert_eq!(
            Switch {
                expression: Expression::Literal(Literal {
                    literal: "3".to_string(),
                    yultype: None,
                }),
                cases: vec![
                    Case {
                        literal: Some(Literal {
                            literal: "1".to_string(),
                            yultype: None,
                        }),
                        block: Block { statements: vec![] },
                    },
                    Case {
                        literal: None,
                        block: Block { statements: vec![] },
                    },
                ],
            }.to_string(),
            "switch 3 case 1 { } default { } "
        );
    }

    #[test]
    fn forloop() {
        assert_eq!(
            ForLoop {
                pre: Block { statements: vec![] },
                condition: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                }),
                post: Block { statements: vec![] },
                body: Block { statements: vec![] },
            }.to_string(),
            "for { } 1 { } { }"
        );
    }
}
