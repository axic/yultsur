use std::collections::HashMap;
use visitor::ASTModifier;
use yul::*;

pub fn resolve(ast: &mut Block) {
    Resolver::new().visit_block(ast);
}

struct Resolver {
    active_variables: Vec<HashMap<String, u64>>,
    active_functions: Vec<HashMap<String, u64>>,
}

fn find_symbol(table: &[HashMap<String, u64>], symbol: &String) -> Option<u64> {
    for map in table.iter().rev() {
        if let Some(id) = map.get(symbol) {
            return Some(*id);
        }
    }
    None
}

impl Resolver {
    fn new() -> Resolver {
        Resolver {
            active_variables: Vec::new(),
            active_functions: Vec::new(),
        }
    }
    fn activate_variable(&mut self, symbol: &Identifier) {
        // TODO error handling: the variable should not already be there.
        self.active_variables
            .last_mut()
            .unwrap()
            .insert(symbol.name.clone(), symbol.id.unwrap());
    }
    fn resolve(&self, symbol: &String) -> u64 {
        // TODO error handling?
        // TODO we should not find it in both.
        if let Some(id) = find_symbol(&self.active_variables, symbol) {
            return id;
        }
        if let Some(id) = find_symbol(&self.active_functions, symbol) {
            return id;
        }
        assert!(false);
        0
    }
}

impl ASTModifier for Resolver {
    fn enter_block(&mut self, block: &mut Block) {
        self.active_variables.push(HashMap::new());
        self.active_functions.push(HashMap::new());
        for st in &block.statements {
            if let Statement::FunctionDefinition(f) = st {
                self.active_functions
                    .last_mut()
                    .unwrap()
                    .insert(f.name.name.clone(), f.name.id.unwrap());
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
        if identifier.id == None {
            identifier.id = Some(self.resolve(&identifier.name));
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
