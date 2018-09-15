extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

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

fn main() {
    smoke();
}

fn smoke() {
    let source = file_to_string("examples/example1.yul");
    let pairs = BlockParser::parse(Rule::block, &source).unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {

        let span = pair.clone().into_span();
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", span);
        println!("Text:    {}", span.as_str());

        for inner_pair in pair.into_inner() {
            let inner_span = inner_pair.clone().into_span();
            match inner_pair.as_rule() {
                Rule::block => println!("Block:  {}", inner_span.as_str()),
                Rule::statement => println!("Statement:  {}", inner_span.as_str()),
                Rule::function_definition => println!("FunctionDefinition:  {}", inner_span.as_str()),
                _ => unreachable!()
            };
        }
    }
}
