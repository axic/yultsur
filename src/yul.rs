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
    Custom(String),
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
pub struct Identifier {
    /// Unique identifier. None for references just after parsing,
    /// but filled after reference resolving.
    pub id: Option<u64>,
    pub name: String,
    pub yultype: Option<Type>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct FunctionCall {
    pub function: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub returns: Vec<Identifier>,
    pub body: Block,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct VariableDeclaration {
    pub variables: Vec<Identifier>,
    pub value: Option<Expression>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Assignment {
    pub variables: Vec<Identifier>,
    pub value: Expression,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    FunctionCall(FunctionCall),
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct If {
    pub condition: Expression,
    pub body: Block,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Case {
    pub literal: Option<Literal>,
    pub body: Block,
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
    Leave,
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
    pub fn new(identifier: &str, id: Option<u64>) -> Self {
        Identifier {
            id,
            name: identifier.to_string(),
            yultype: None,
        }
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
        write!(f, "{}", self.literal)?;
        if let Some(yultype) = &self.yultype {
            write!(f, ":{}", yultype)
        } else {
            write!(f, "")
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(yultype) = &self.yultype {
            write!(f, ":{}", yultype)
        } else {
            write!(f, "")
        }
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.function)?;
        write!(
            f,
            "{}",
            self.arguments
                .iter()
                .map(|argument| format!("{}", argument))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        write!(f, ")")
    }
}

impl fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}(", self.name)?;
        write!(
            f,
            "{}",
            self.parameters
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        write!(f, ")")?;
        if self.returns.len() > 0 {
            write!(f, " -> ")?;
            write!(
                f,
                "{}",
                self.returns
                    .iter()
                    .map(|identifier| format!("{}", identifier))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        write!(f, " {}", self.body)
    }
}

impl fmt::Display for VariableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.variables.len() == 0 {
            panic!("VariableDeclaration must have identifiers")
        }
        write!(f, "let ")?;
        write!(
            f,
            "{}",
            self.variables
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        if let Some(expression) = &self.value {
            write!(f, " := {}", expression)
        } else {
            write!(f, "")
        }
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.variables.len() == 0 {
            panic!("Assignment must have identifiers")
        }
        write!(
            f,
            "{}",
            self.variables
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        write!(f, " := {}", self.value)
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
        write!(f, "if {} {}", self.condition, self.body)
    }
}

impl fmt::Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(literal) = &self.literal {
            // FIXME: should validate this on the new/default trait
            if literal.literal.len() == 0 {
                panic!("Case with literal should not be empty");
            }
            write!(f, "case {} {}", literal, self.body)
        } else {
            write!(f, "default {}", self.body)
        }
    }
}

impl fmt::Display for Switch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "switch {}", self.expression)?;
        for case in &self.cases {
            write!(f, " {}", case)?;
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
            Statement::Leave => write!(f, "leave"),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        for (_, statement) in self.statements.iter().enumerate() {
            write!(f, " {}", statement)?;
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
            }
            .to_string(),
            "testliteral"
        );
    }

    #[test]
    fn literal_typed() {
        assert_eq!(
            Literal {
                literal: "testliteral".to_string(),
                yultype: Some(Type::Uint256),
            }
            .to_string(),
            "testliteral:u256"
        );
    }

    #[test]
    fn literal_custom_typed() {
        assert_eq!(
            Literal {
                literal: "testliteral".to_string(),
                yultype: Some(Type::Custom("memptr".to_string())),
            }
            .to_string(),
            "testliteral:memptr"
        );
    }

    #[test]
    fn identifier() {
        assert_eq!(
            Identifier {
                id: None,
                name: "testidentifier".to_string(),
                yultype: None,
            }
            .to_string(),
            "testidentifier"
        );
    }

    #[test]
    fn identifier_typed() {
        assert_eq!(
            Identifier {
                id: Some(1),
                name: "testidentifier".to_string(),
                yultype: Some(Type::Uint256),
            }
            .to_string(),
            "testidentifier:u256"
        );
    }

    #[test]
    fn identifierr_custom_typed() {
        assert_eq!(
            Identifier {
                id: Some(1),
                name: "testidentifier".to_string(),
                yultype: Some(Type::Custom("memptr".to_string())),
            }
            .to_string(),
            "testidentifier:memptr"
        );
    }

    #[test]
    fn functioncall() {
        assert_eq!(
            FunctionCall {
                function: Identifier {
                    id: None,
                    name: "test".to_string(),
                    yultype: None,
                },
                arguments: vec![
                    Expression::Identifier(Identifier {
                        id: None,
                        name: "test".to_string(),
                        yultype: None,
                    }),
                    Expression::Literal(Literal {
                        literal: "literal".to_string(),
                        yultype: None,
                    }),
                ],
            }
            .to_string(),
            "test(test, literal)"
        );
    }

    #[test]
    fn if_statement() {
        assert_eq!(
            If {
                condition: Expression::Literal(Literal {
                    literal: "literal".to_string(),
                    yultype: None,
                }),
                body: Block { statements: vec![] },
            }
            .to_string(),
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
            }
            .to_string(),
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
            }
            .to_string(),
            "{ literal }"
        );
    }

    #[test]
    fn assignment_single() {
        assert_eq!(
            Assignment {
                variables: vec![Identifier {
                    id: None,
                    name: "a".to_string(),
                    yultype: None,
                }],
                value: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                }),
            }
            .to_string(),
            "a := 1"
        );
    }

    #[test]
    fn assignment_multi() {
        assert_eq!(
            Assignment {
                variables: vec![
                    Identifier {
                        id: None,
                        name: "a".to_string(),
                        yultype: None,
                    },
                    Identifier {
                        id: None,
                        name: "b".to_string(),
                        yultype: None,
                    },
                    Identifier {
                        id: None,
                        name: "c".to_string(),
                        yultype: None,
                    },
                ],
                value: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                }),
            }
            .to_string(),
            "a, b, c := 1"
        );
    }

    #[test]
    fn variabledeclaration_empty() {
        assert_eq!(
            VariableDeclaration {
                variables: vec![Identifier {
                    id: Some(1),
                    name: "a".to_string(),
                    yultype: None,
                }],
                value: None,
            }
            .to_string(),
            "let a"
        );
    }

    #[test]
    fn variabledeclaration_single() {
        assert_eq!(
            VariableDeclaration {
                variables: vec![Identifier {
                    id: Some(1),
                    name: "a".to_string(),
                    yultype: None,
                }],
                value: Some(Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                })),
            }
            .to_string(),
            "let a := 1"
        );
    }

    #[test]
    fn variabledeclaration_multi() {
        assert_eq!(
            VariableDeclaration {
                variables: vec![
                    Identifier {
                        id: Some(1),
                        name: "a".to_string(),
                        yultype: None,
                    },
                    Identifier {
                        id: Some(2),
                        name: "b".to_string(),
                        yultype: None,
                    },
                    Identifier {
                        id: Some(3),
                        name: "c".to_string(),
                        yultype: None,
                    },
                ],
                value: Some(Expression::Literal(Literal {
                    literal: "1".to_string(),
                    yultype: None,
                })),
            }
            .to_string(),
            "let a, b, c := 1"
        );
    }

    #[test]
    fn functiondefinition_basic() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: Some(1),
                    name: "name".to_string(),
                    yultype: None,
                },
                parameters: vec![],
                returns: vec![],
                body: Block { statements: vec![] },
            }
            .to_string(),
            "function name() { }"
        );
    }

    #[test]
    fn functiondefinition_single_arg() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: Some(1),
                    name: "name".to_string(),
                    yultype: None,
                },
                parameters: vec![Identifier {
                    id: None,
                    name: "a".to_string(),
                    yultype: None,
                }],
                returns: vec![],
                body: Block { statements: vec![] },
            }
            .to_string(),
            "function name(a) { }"
        );
    }

    #[test]
    fn functiondefinition_single_ret() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: Some(1),
                    name: "name".to_string(),
                    yultype: None,
                },
                parameters: vec![],
                returns: vec![Identifier {
                    id: Some(2),
                    name: "a".to_string(),
                    yultype: None,
                }],
                body: Block { statements: vec![] },
            }
            .to_string(),
            "function name() -> a { }"
        );
    }

    #[test]
    fn functiondefinition_multi() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: Some(1),
                    name: "name".to_string(),
                    yultype: None,
                },
                parameters: vec![
                    Identifier {
                        id: Some(2),
                        name: "a".to_string(),
                        yultype: None,
                    },
                    Identifier {
                        id: Some(3),
                        name: "b".to_string(),
                        yultype: None,
                    },
                ],
                returns: vec![
                    Identifier {
                        id: Some(1),
                        name: "c".to_string(),
                        yultype: None,
                    },
                    Identifier {
                        id: Some(2),
                        name: "d".to_string(),
                        yultype: None,
                    },
                ],
                body: Block { statements: vec![] },
            }
            .to_string(),
            "function name(a, b) -> c, d { }"
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
                body: Block { statements: vec![] },
            }
            .to_string(),
            "case literal { }"
        );
    }

    #[test]
    fn case_default() {
        assert_eq!(
            Case {
                literal: None,
                body: Block { statements: vec![] },
            }
            .to_string(),
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
                        body: Block { statements: vec![] },
                    },
                    Case {
                        literal: None,
                        body: Block { statements: vec![] },
                    },
                ],
            }
            .to_string(),
            "switch 3 case 1 { } default { }"
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
            }
            .to_string(),
            "for { } 1 { } { }"
        );
    }
    #[test]
    fn leave() {
        assert_eq!(
            Block {
                statements: vec![Statement::Leave]
            }
            .to_string(),
            "{ leave }"
        );
    }
}
