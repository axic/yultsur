use yul::*;

use pest::Parser;
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "yul.pest"]
struct BlockParser;

use std::fs::File;
use std::io::prelude::*;

fn file_to_string(path: &str) -> String {
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    content
}

impl Identifier {
    fn from(pair: Pair<Rule>) -> String {
        pair.as_str().to_string()
    }

    fn from_untyped(pair: Pair<Rule>) -> Identifier {
        let identifier = Identifier::from(pair);

        Identifier {
            identifier,
            yultype: None
        }
    }

    fn from_typed(pair: Pair<Rule>) -> Identifier {
        let mut token_iter = pair.into_inner();
        let identifier = Identifier::from(token_iter.next().unwrap());
        let yultype = token_iter.next().map(|t| Type::from(t));

        Identifier {
            identifier,
            yultype
        }
    }

    fn list(pair: Pair<Rule>) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::identifier => {
                    identifiers.push(Identifier::from_untyped(p));
                }
                _ => unreachable!()
            }
        }
        identifiers
    }

    fn list_typed(pair: Pair<Rule>) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::typed_identifier => {
                    identifiers.push(Identifier::from_typed(p));
                }
                _ => unreachable!()
            }
        }
        identifiers
    }
}

impl Type {
    fn from(pair: Pair<Rule>) -> Type {
        let current = pair.into_inner().next().unwrap();
        match current.as_rule() {
            Rule::identifier => Type::Custom(Identifier::from(current)),
            Rule::builtin_typename => match current.as_str() {
                "bool" => Type::Bool,
                "u8" => Type::Uint8,
                "u32" => Type::Uint32,
                "u64" => Type::Uint64,
                "u128" => Type::Uint128,
                "u256" => Type::Uint256,
                "s8" => Type::Int8,
                "s32" => Type::Int32,
                "s64" => Type::Int64,
                "s128" => Type::Int128,
                "s256" => Type::Int256,
                _ => unreachable!()
            },
            _ => unreachable!()
        }
    }
}

impl Literal {
    fn from(pair: Pair<Rule>) -> Literal {
        let mut token_iter = pair.into_inner();
        let literal = Identifier::from(token_iter.next().unwrap());
        let yultype = token_iter.next().map(|t| Type::from(t));

        Literal {
            literal,
            yultype
        }
    }
}

impl FunctionCall {
    fn from(pair: Pair<Rule>) -> FunctionCall {
        let mut token_iter = pair.into_inner();
        let identifier = Identifier::from_untyped(token_iter.next().unwrap());
        let arguments = token_iter.map(|p| match p.as_rule() {
            Rule::expression => Expression::from(p),
            _ => unreachable!()
        }).collect();

        FunctionCall {
            identifier,
            arguments
        }
    }
}

impl Expression {
    fn from(pair: Pair<Rule>) -> Expression {
        let mut token_iter = pair.into_inner();
        let p = token_iter.next().unwrap();
        match p.as_rule()  {
            Rule::function_call => Expression::FunctionCall(FunctionCall::from(p)),
            Rule::identifier => Expression::Identifier(Identifier::from_untyped(p)),
            Rule::literal => Expression::Literal(Literal::from(p)),
            _ => unreachable!()
        }
    }
}

impl Case {
    fn from(pair: Pair<Rule>) -> Case {
        let mut token_iter = pair.into_inner();
        let literal = Literal::from(token_iter.next().unwrap());
        let block = Block::from(token_iter.next().unwrap());

        Case {
            literal: Some(literal),
            block
        }
    }

    fn from_default(pair: Pair<Rule>) -> Case {
        let mut token_iter = pair.into_inner();
        let block = Block::from(token_iter.next().unwrap());

        Case {
            literal: None,
            block
        }
    }
}


impl Switch {
    fn from(pair: Pair<Rule>) -> Switch {
        let mut token_iter = pair.into_inner();
        let expression = Expression::from(token_iter.next().unwrap());
        let cases = token_iter.map(|p| { match p.as_rule() {
            Rule::case => Case::from(p),
            Rule::default => Case::from_default(p),
            _ => unreachable!()
        }}).collect();

        Switch {
            expression,
            cases
        }
    }
}

impl Assignment {
    fn from(pair: Pair<Rule>) -> Assignment {
        let mut token_iter = pair.into_inner();
        let identifiers = Identifier::list(token_iter.next().unwrap());
        let expression = Expression::from(token_iter.next().unwrap());

        Assignment {
            identifiers,
            expression
        }
    }
}

impl VariableDeclaration {
    fn from(pair: Pair<Rule>) -> VariableDeclaration {
        let mut token_iter = pair.into_inner();

        let identifiers = Identifier::list(token_iter.next().unwrap());
        let expression = token_iter.next().map(|e| Expression::from(e));

        VariableDeclaration {
            identifiers,
            expression
        }
    }
}

impl FunctionDefinition {
    fn from(pair: Pair<Rule>) -> FunctionDefinition {
        let mut token_iter = pair.into_inner();
        let name = Identifier::from_untyped(token_iter.next().unwrap());

        let current = token_iter.next().unwrap();
        let (parameters, current) = match current.as_rule() {
            Rule::typed_parameter_list => (Identifier::list_typed(current), token_iter.next().unwrap()),
            Rule::untyped_parameter_list => (Identifier::list(current), token_iter.next().unwrap()),
            _ => (vec![], current)
        };
        let (returns, current) = match current.as_rule() {
            Rule::typed_identifier_list => (Identifier::list_typed(current), token_iter.next().unwrap()),
            Rule::untyped_identifier_list => (Identifier::list(current), token_iter.next().unwrap()),
            _ => (vec![], current)
        };
        let block = Block::from(current);

        FunctionDefinition {
            name,
            parameters,
            returns,
            block
        }
    }
}

impl If {
    fn from(pair: Pair<Rule>) -> If {
        let mut token_iter = pair.into_inner();
        let expression = Expression::from(token_iter.next().unwrap());
        let block = Block::from(token_iter.next().unwrap());

        If {
            expression,
            block
        }
    }
}

impl ForLoop {
    fn from(pair: Pair<Rule>) -> ForLoop {
        let mut token_iter = pair.into_inner();
        let pre = Block::from(token_iter.next().unwrap());
        let condition = Expression::from(token_iter.next().unwrap());
        let post = Block::from(token_iter.next().unwrap());
        let body = Block::from(token_iter.next().unwrap());

        ForLoop {
            pre,
            condition,
            post,
            body
        }
    }
}

impl Statement {
    fn from(pair: Pair<Rule>) -> Statement {
        let mut token_iter = pair.into_inner();
        let p = token_iter.next().unwrap();
        match p.as_rule() {
            Rule::block => Statement::Block(Block::from(p)),
            Rule::function_definition => Statement::FunctionDefinition(FunctionDefinition::from(p)),
            Rule::variable_declaration => Statement::VariableDeclaration(VariableDeclaration::from(p)),
            Rule::assignment => Statement::Assignment(Assignment::from(p)),
            Rule::expression => Statement::Expression(Expression::from(p)),
            Rule::switch => Statement::Switch(Switch::from(p)),
            Rule::if_statement => Statement::If(If::from(p)),
            Rule::for_loop => Statement::ForLoop(ForLoop::from(p)),
            Rule::break_statement => Statement::Break,
            Rule::continue_statement => Statement::Continue,
            Rule::leave => Statement::Leave,
            _ => unreachable!()
        }
    }
}

impl Block {
    fn from(pair: Pair<Rule>) -> Block {
        let mut statements: Vec<Statement> = vec![];
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::statement => {
                    statements.push(Statement::from(p));
                }
                _ => unreachable!()
            }
        }
        Block { statements }
    }
}

pub fn parse_block(source: &str) -> Block {
    let mut pairs = BlockParser::parse(Rule::block, &source).unwrap();
    Block::from(pairs.next().unwrap())
}

#[cfg(test)]
mod tests {
use super::*;

    #[test]
    fn continue_statement() {
        test_file("examples/continue.yul");
    }

    #[test]
    fn break_statement() {
        test_file("examples/break.yul");
    }

    #[test]
    fn for_loop() {
        test_file("examples/for.yul");
    }

    #[test]
    fn if_statement() {
        test_file("examples/if.yul");
    }

    #[test]
    fn switch() {
        test_file("examples/switch.yul");
    }

    #[test]
    fn assignment() {
        test_file("examples/assignment.yul");
    }

    #[test]
    fn var_decl() {
        test_file("examples/var_decl.yul");
    }

    #[test]
    fn empty_nested_blocks() {
        test_file("examples/nested_blocks.yul");
    }

    #[test]
    fn empty_function() {
        test_file("examples/power_function_signature.yul");
    }

    #[test]
    fn empty_block() {
        test_file("examples/empty_block.yul");
    }

    #[test]
    fn untyped() {
        test_file("examples/untyped.yul");
    }

    #[test]
    fn function_call() {
        test_file("examples/function_call.yul");
    }

    #[test]
    fn leave() {
        test_file("examples/leave.yul");
    }

    fn test_file(filename: &str) {
        let source = file_to_string(filename);
        let block = parse_block(&source);
        assert_eq!(
            source,
            block.to_string()
        );
    }
}
