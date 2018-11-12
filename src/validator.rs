use yul::*;

pub type Result<T> = std::result::Result<T, &'static str>;

pub trait Validator: Send + Sync {
    fn validate(&self, typed: bool) -> Result<()>;
}

impl Validator for Type {
    fn validate(&self, typed: bool) -> Result<()> {
        match self {
            Type::Custom(custom) => custom.validate(typed),
            _ => Ok(()),
        }
    }
}

impl Validator for Literal {
    fn validate(&self, typed: bool) -> Result<()> {
        if typed {
            if self.yultype == None {
                return Err("Type must be set");
            }
            if let Some(yultype) = &self.yultype {
                yultype.validate(typed)?;
            }
            // FIXME: validate that the literal is valid based on the type
        }
        Ok(())
    }
}

impl Validator for Identifier {
    fn validate(&self, _: bool) -> Result<()> {
        let mut bytes = self.0.bytes();

        match bytes.next() {
            None => return Err("Identifier label cannot be empty"),
            Some(byte) => match byte {
                b'a'...b'z' |
                b'A'...b'Z' |
                b'_' | b'$' => {},
                _ => return Err("Identifier label must start with a letter, `_`, or `$` character")
            }
        }

        for byte in bytes {
            match byte {
                b'a'...b'z' |
                b'A'...b'Z' |
                b'0'...b'9' |
                b'_' | b'$' => {},
                _ => return Err("Identifier label can contain only letters, digits, `_`, or `$` characters")
            }
        }

        Ok(())
    }
}

impl Validator for TypedIdentifier {
    fn validate(&self, typed: bool) -> Result<()> {
        if typed {
            self.yultype.validate(typed)?;
        }
        self.identifier.validate(typed)
    }
}

impl Validator for Block {
    fn validate(&self, typed: bool) -> Result<()> {
        for statement in &self.statements {
            statement.validate(typed)?;
        }
        Ok(())
    }
}

impl Validator for Statement {
    fn validate(&self, typed: bool) -> Result<()> {
        match *self {
            Statement::Switch(ref switch) => switch.validate(typed),
            _ => Ok(()),
        }
    }
}

impl Validator for Case {
    fn validate(&self, typed: bool) -> Result<()> {
        if let Some(literal) = &self.literal {
            literal.validate(typed)?;
            if literal.literal.len() == 0 {
                return Err("Case literal cannot be empty");
            }
        }
        Ok(())
    }
}

impl Validator for Switch {
    fn validate(&self, typed: bool) -> Result<()> {
        for case in &self.cases {
            case.validate(typed)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifiers() {
        assert!(Identifier::new("foo").validate(true).is_ok());
        assert!(Identifier::new("Foo").validate(true).is_ok());
        assert!(Identifier::new("_foo").validate(true).is_ok());
        assert!(Identifier::new("$foo").validate(true).is_ok());
        assert!(Identifier::new("f00").validate(true).is_ok());

        assert!(Identifier::new("foo_bar").validate(true).is_ok());
        assert!(Identifier::new("FooBar").validate(true).is_ok());

        assert!(Identifier::new("1foo").validate(true).is_err());
        assert!(Identifier::new("#foo").validate(true).is_err());
    }

    #[test]
    fn basic_type() {
        assert!(Type::Bool.validate(true).is_ok(), "");
    }

    #[test]
    fn custom_type() {
        assert!(
            Type::Custom("test".into()).validate(true).is_ok(),
            ""
        );
    }

    #[test]
    fn invalid_custom_type() {
        assert!(
            Type::Custom("test invalid type".into())
                .validate(true)
                .is_err(),
            ""
        );
    }

    #[test]
    fn untyped_literal() {
        assert!(
            Literal {
                literal: "test".to_string(),
                yultype: None
            }.validate(false)
                .is_ok(),
            ""
        );
        assert!(
            Literal {
                literal: "test".to_string(),
                yultype: None
            }.validate(true)
                .is_err(),
            ""
        );
    }

    #[test]
    fn typed_literal() {
        assert!(
            Literal {
                literal: "test".to_string(),
                yultype: Some(Type::Bool)
            }.validate(true)
                .is_ok(),
            ""
        );
    }

    #[test]
    fn case_invalid_default() {
        assert!(
            Case {
                literal: Some(Literal {
                    literal: "".to_string(),
                    yultype: None,
                }),
                block: Block { statements: vec![] },
            }.validate(false)
                .is_err(),
            ""
        );
    }

    #[test]
    fn complex_example() {
        assert!(
            Block {
                statements: vec![Statement::Switch(Switch {
                    expression: Expression::Identifier("shouldbebool".into()),
                    cases: vec![
                        Case {
                            literal: Some(Literal {
                                literal: "true".to_string(),
                                yultype: Some(Type::Bool),
                            }),
                            block: Block { statements: vec![] },
                        },
                        Case {
                            literal: None,
                            block: Block { statements: vec![] },
                        },
                    ],
                })],
            }.validate(false)
                .is_ok(),
            ""
        );
    }
}
