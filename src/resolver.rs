use std::collections::HashMap;
use visitor::ASTModifier;
use yul::*;

use dialect::Dialect;

/// Resolves all references in the given AST and returns a
/// hash map from id to function signature for each user-defined function.
pub fn resolve<D: Dialect>(ast: &mut Block) -> HashMap<u64, FunctionSignature> {
    let mut r = Resolver::<D>::new();
    r.visit_block(ast);
    std::mem::take(&mut r.function_signatures)
}

struct Resolver<D: Dialect> {
    active_variables: Vec<HashMap<String, u64>>,
    active_functions: Vec<HashMap<String, u64>>,
    function_signatures: HashMap<u64, FunctionSignature>,
    // TODO we should not need that.
    dialect: D,
}

pub struct FunctionSignature {
    pub parameters: u64,
    pub returns: u64,
}

fn find_symbol(table: &[HashMap<String, u64>], symbol: &String) -> Option<u64> {
    for map in table.iter().rev() {
        if let Some(id) = map.get(symbol) {
            return Some(*id);
        }
    }
    None
}

impl<D: Dialect> Resolver<D> {
    fn new() -> Resolver<D> {
        Resolver::<D> {
            active_variables: Vec::new(),
            active_functions: Vec::new(),
            function_signatures: HashMap::new(),
            dialect: D::new(),
        }
    }
    fn activate_variable(&mut self, symbol: &Identifier) {
        // TODO error handling: the variable should not already be there.
        if let IdentifierID::Declaration(id) = symbol.id {
            self.active_variables
                .last_mut()
                .unwrap()
                .insert(symbol.name.clone(), id);
        } else {
            panic!()
        }
    }
    fn resolve(&self, symbol: &String) -> IdentifierID {
        if D::is_builtin(symbol.as_str()) {
            return IdentifierID::BuiltinReference;
        }
        // TODO error handling?
        // TODO we should not find it in both.
        if let Some(id) = find_symbol(&self.active_variables, symbol) {
            return IdentifierID::Reference(id);
        }
        if let Some(id) = find_symbol(&self.active_functions, symbol) {
            return IdentifierID::Reference(id);
        }
        assert!(false);
        IdentifierID::UnresolvedReference
    }
}

impl<D: Dialect> ASTModifier for Resolver<D> {
    fn enter_block(&mut self, block: &mut Block) {
        self.active_variables.push(HashMap::new());
        self.active_functions.push(HashMap::new());
        for st in &block.statements {
            if let Statement::FunctionDefinition(f) = st {
                if let IdentifierID::Declaration(id) = f.name.id {
                    self.active_functions
                        .last_mut()
                        .unwrap()
                        .insert(f.name.name.clone(), id);
                    self.function_signatures.insert(
                        id,
                        FunctionSignature {
                            parameters: f.parameters.len() as u64,
                            returns: f.returns.len() as u64,
                        },
                    );
                } else {
                    panic!()
                }
            }
        }
    }
    fn exit_block(&mut self, _block: &mut Block) {
        self.active_variables.pop();
        self.active_functions.pop();
    }
    fn exit_variable_declaration(&mut self, variables: &mut VariableDeclaration) {
        for var in &variables.variables {
            self.activate_variable(var);
        }
    }
    fn exit_identifier(&mut self, mut identifier: &mut Identifier) {
        if identifier.id == IdentifierID::UnresolvedReference {
            identifier.id = self.resolve(&identifier.name);
        }
    }
    fn visit_function_definition(&mut self, fun_def: &mut FunctionDefinition) {
        let outer_variables = std::mem::take(&mut self.active_variables);
        self.active_variables.push(HashMap::new());
        for var in fun_def.parameters.iter().chain(fun_def.returns.iter()) {
            self.activate_variable(var);
        }
        self.visit_block(&mut fun_def.body);
        self.active_variables.pop();
        self.active_variables = outer_variables;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use dialect::EVMDialect;

    #[test]
    fn with_dialect() {
        let mut ast = Block {
            statements: vec![Statement::Expression(Expression::FunctionCall(
                FunctionCall {
                    function: Identifier {
                        id: IdentifierID::UnresolvedReference,
                        name: "add".to_string(),
                        yultype: None,
                    },
                    arguments: vec![],
                },
            ))],
        };
        resolve::<EVMDialect>(&mut ast);
    }
}
