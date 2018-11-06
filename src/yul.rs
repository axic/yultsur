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
    pub literal: Box<str>,
    pub yultype: Type,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Identifier(pub Box<str>);

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

impl<S: Into<Box<str>>> From<S> for Identifier {
    fn from(string: S) -> Self {
        Identifier(string.into())
    }
}

impl Identifier {
    pub fn new(identifier: &str) -> Self {
        identifier.into()
    }
}

macro_rules! impl_literal_from {
    ($( $yultype:ident : $type:ty ),*) => {
        $(
            impl From<$type> for Literal {
                fn from(val: $type) -> Literal {
                    Literal {
                        literal: val.to_string().into(),
                        yultype: Type::$yultype
                    }
                }
            }
        )*
    }
}

impl_literal_from!(
    Uint8:u8, Uint32:u16, Uint32:u32, Uint64:u64, Uint128:u128,
    Int8:i8,  Int32:i16,  Int32:i32,  Int64:i64,  Int128:i128,
    Bool:bool
);

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.literal, self.yultype)
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

fn write_list<T, I>(f: &mut fmt::Formatter, list: I) -> fmt::Result
where
    T: fmt::Display,
    I: IntoIterator<Item = T>,
{
    let mut items = list.into_iter();

    if let Some(item) = items.next() {
        write!(f, "{}", item)?;

        for item in items {
            write!(f, ", {}", item)?;
        }
    }

    Ok(())
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.identifier)?;
        write_list(f, &self.arguments)?;
        write!(f, ")")
    }
}

impl fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}(", self.name)?;
        write_list(f, &self.parameters)?;
        write!(f, ")")?;
        if self.returns.len() > 0 {
            write!(f, " -> ")?;
            write_list(f, &self.returns)?;
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
        write!(f, "let ")?;
        write_list(f, &self.identifiers)?;
        if let Some(expression) = &self.expression {
            write!(f, " := {}", expression)
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.identifiers.len() == 0 {
            panic!("Assignment must have identifiers")
        }
        write_list(f, &self.identifiers)?;
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
        write!(f, "switch {} ", self.expression)?;
        for case in &self.cases {
            write!(f, "{} ", case)?;
        }
        Ok(())
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
        write!(f, "{{")?;
        for statement in &self.statements {
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
                literal: "testliteral".into(),
                yultype: Type::Uint256,
            }.to_string(),
            "testliteral:u256"
        );
    }

    #[test]
    fn literal_custom_typed() {
        assert_eq!(
            Literal {
                literal: "testliteral".into(),
                yultype: Type::Custom("memptr".into()),
            }.to_string(),
            "testliteral:memptr"
        );
    }

    #[test]
    fn identifier() {
        assert_eq!(
            Identifier::from("testidentifier").to_string(),
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
                identifier: "fun".into(),
                arguments: vec![
                    Expression::Identifier("test".into()),
                    Expression::Literal(true.into()),
                ],
            }.to_string(),
            "fun(test, true:bool)"
        );
    }

    #[test]
    fn if_statement() {
        assert_eq!(
            If {
                expression: Expression::Literal(true.into()),
                block: Block { statements: vec![] },
            }.to_string(),
            "if true:bool { }"
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
                statements: vec![Statement::Expression(Expression::Literal(true.into()))],
            }.to_string(),
            "{ true:bool }"
        );
    }

    #[test]
    fn assignment_single() {
        assert_eq!(
            Assignment {
                identifiers: vec![Identifier::from("a")],
                expression: Expression::Literal(1u8.into()),
            }.to_string(),
            "a := 1:u8"
        );
    }

    #[test]
    fn assignment_multi() {
        assert_eq!(
            Assignment {
                identifiers: vec![
                    Identifier::from("a"),
                    Identifier::from("b"),
                    Identifier::from("c"),
                ],
                expression: Expression::Literal(1u8.into()),
            }.to_string(),
            "a, b, c := 1:u8"
        );
    }

    #[test]
    fn variabledeclaration_empty() {
        assert_eq!(
            VariableDeclaration {
                identifiers: vec![TypedIdentifier {
                    identifier: "a".into(),
                    yultype: Type::Uint8,
                }],
                expression: None,
            }.to_string(),
            "let a:u8"
        );
    }

    #[test]
    fn variabledeclaration_single() {
        assert_eq!(
            VariableDeclaration {
                identifiers: vec![TypedIdentifier {
                    identifier: "a".into(),
                    yultype: Type::Uint8,
                }],
                expression: Some(Expression::Literal(1u8.into())),
            }.to_string(),
            "let a:u8 := 1:u8"
        );
    }

    #[test]
    fn variabledeclaration_multi() {
        assert_eq!(
            VariableDeclaration {
                identifiers: vec![
                    TypedIdentifier {
                        identifier: "a".into(),
                        yultype: Type::Uint8,
                    },
                    TypedIdentifier {
                        identifier: "b".into(),
                        yultype: Type::Uint8,
                    },
                    TypedIdentifier {
                        identifier: "c".into(),
                        yultype: Type::Uint8,
                    },
                ],
                expression: Some(Expression::Literal(1u8.into())),
            }.to_string(),
            "let a:u8, b:u8, c:u8 := 1:u8"
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
                    yultype: Type::Bool,
                }],
                returns: vec![],
                block: Block { statements: vec![] },
            }.to_string(),
            "function name(a:bool) { }"
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
                    yultype: Type::Bool,
                }],
                block: Block { statements: vec![] },
            }.to_string(),
            "function name() -> a:bool { }"
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
                        yultype: Type::Bool,
                    },
                    TypedIdentifier {
                        identifier: "b".into(),
                        yultype: Type::Bool,
                    },
                ],
                returns: vec![
                    TypedIdentifier {
                        identifier: "c".into(),
                        yultype: Type::Bool,
                    },
                    TypedIdentifier {
                        identifier: "d".into(),
                        yultype: Type::Bool,
                    },
                ],
                block: Block { statements: vec![] },
            }.to_string(),
            "function name(a:bool, b:bool) -> c:bool, d:bool { }"
        );
    }

    #[test]
    fn case() {
        assert_eq!(
            Case {
                literal: Some(42u16.into()), // will be cast to u32 in Yul
                block: Block { statements: vec![] },
            }.to_string(),
            "case 42:u32 { }"
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
                    literal: "3".into(),
                    yultype: Type::Uint8,
                }),
                cases: vec![
                    Case {
                        literal: Some(1u8.into()),
                        block: Block { statements: vec![] },
                    },
                    Case {
                        literal: None,
                        block: Block { statements: vec![] },
                    },
                ],
            }.to_string(),
            "switch 3:u8 case 1:u8 { } default { } "
        );
    }

    #[test]
    fn forloop() {
        assert_eq!(
            ForLoop {
                pre: Block { statements: vec![] },
                condition: Expression::Literal(1u8.into()),
                post: Block { statements: vec![] },
                body: Block { statements: vec![] },
            }.to_string(),
            "for { } 1:u8 { } { }"
        );
    }
}
