#[derive(Hash,PartialEq,Debug)]
pub struct Block {
  statements: Vec<Statement>
}

#[derive(Hash,PartialEq,Debug)]
pub struct Literal {
  literal: String
}

#[derive(Hash,PartialEq,Debug)]
pub struct Identifier {
  identifier: String
}

#[derive(Hash,PartialEq,Debug)]
pub enum Expression {
  Literal(Literal),
  Identifier(Identifier),
  FunctionCall(Identifier, Vec<Expression>),
}

#[derive(Hash,PartialEq,Debug)]
pub struct Case {
  literal: Literal,
  block: Block
}

#[derive(Hash,PartialEq,Debug)]
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
