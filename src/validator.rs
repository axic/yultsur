use yul::*;

pub trait Validator: Send + Sync {
    fn validate(&self, typed: bool) -> Result<(), String>;
}

impl Validator for Type {
    fn validate(&self, typed: bool) -> Result<(), String> {
        match *self {
            // FIXME: validate that custom type name doesn't have space, etc.
            Type::Custom(ref custom) => Ok(()),
            _ => Ok(()),
        }
    }
}

impl Validator for Literal {
    fn validate(&self, typed: bool) -> Result<(), String> {
        if typed {
            if self.yultype == None {
                return Err("Type must be set".to_string());
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
    fn validate(&self, typed: bool) -> Result<(), String> {
        if typed {
            if self.yultype == None {
                return Err("Type must be set".to_string());
            }
            if let Some(yultype) = &self.yultype {
                yultype.validate(typed)?;
            }
        }
        Ok(())
    }
}

impl Validator for Block {
    fn validate(&self, typed: bool) -> Result<(), String> {
        for statement in &self.statements {
            statement.validate(typed)?;
        }
        Ok(())
    }
}

impl Validator for Statement {
    fn validate(&self, typed: bool) -> Result<(), String> {
        match *self {
            Statement::Switch(ref switch) => switch.validate(typed),
            _ => Ok(()),
        }
    }
}

impl Validator for Case {
    fn validate(&self, typed: bool) -> Result<(), String> {
        if let Some(literal) = &self.literal {
            literal.validate(typed)?;
            if literal.literal.len() == 0 {
                return Err("Case literal cannot be empty".to_string());
            }
        }
        Ok(())
    }
}

impl Validator for Switch {
    fn validate(&self, typed: bool) -> Result<(), String> {
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
    fn basic_type() {
        assert!(!Type::Bool.validate(true).is_err(), "");
    }

    #[test]
    fn custom_type() {
        assert!(
            !Type::Custom("test".to_string()).validate(true).is_err(),
            ""
        );
    }

    #[test]
    fn invalid_custom_type() {
        assert!(
            !Type::Custom("test invalid type".to_string())
                .validate(true)
                .is_err(),
            ""
        );
    }

    #[test]
    fn untyped_literal() {
        assert!(
            !Literal {
                literal: "test".to_string(),
                yultype: None
            }.validate(false)
                .is_err(),
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
            !Literal {
                literal: "test".to_string(),
                yultype: Some(Type::Bool)
            }.validate(true)
                .is_err(),
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
            !Block {
                statements: vec![Statement::Switch(Switch {
                    expression: Expression::Identifier(Identifier {
                        identifier: "shouldbebool".to_string(),
                        yultype: Some(Type::Bool),
                    }),
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
                .is_err(),
            ""
        );
    }
}
