use yul::*;

pub trait ASTVisitor {
    fn enter_statement(&mut self, _st: &Statement) {}
    fn visit_statement(&mut self, st: &Statement) {
        self.enter_statement(st);
        match st {
            Statement::Block(block) => self.visit_block(block),
            Statement::FunctionDefinition(fun) => self.visit_function_definition(fun),
            Statement::VariableDeclaration(variable) => self.visit_variable_declaration(variable),
            Statement::Assignment(assignment) => self.visit_assignment(assignment),
            Statement::Expression(expression) => self.visit_expression(expression),
            Statement::If(if_st) => self.visit_if(if_st),
            Statement::Switch(switch) => self.visit_switch(switch),
            Statement::ForLoop(for_loop) => self.visit_for(for_loop),
            Statement::Break => self.visit_break(),
            Statement::Continue => self.visit_continue(),
            Statement::Leave => self.visit_leave(),
        };
        self.exit_statement(st);
    }
    fn exit_statement(&mut self, _st: &Statement) {}

    fn enter_block(&mut self, _block: &Block) {}
    fn visit_block(&mut self, block: &Block) {
        self.enter_block(block);
        block
            .statements
            .iter()
            .for_each(|st| self.visit_statement(st));
        self.exit_block(block);
    }
    fn exit_block(&mut self, _block: &Block) {}

    fn enter_function_definition(&mut self, _fun_def: &FunctionDefinition) {}
    fn visit_function_definition(&mut self, fun_def: &FunctionDefinition) {
        self.enter_function_definition(fun_def);
        self.visit_identifier(&fun_def.name);
        self.visit_identifier_vector(&fun_def.parameters);
        self.visit_identifier_vector(&fun_def.returns);
        self.visit_block(&fun_def.body);
        self.exit_function_definition(fun_def);
    }
    fn exit_function_definition(&mut self, _fun_def: &FunctionDefinition) {}

    fn enter_variable_declaration(&mut self, _variable: &VariableDeclaration) {}
    fn visit_variable_declaration(&mut self, variable: &VariableDeclaration) {
        self.enter_variable_declaration(variable);
        self.visit_identifier_vector(&variable.variables);
        if let Some(value) = &variable.value {
            self.visit_expression(value);
        }
        self.exit_variable_declaration(variable);
    }
    fn exit_variable_declaration(&mut self, _variable: &VariableDeclaration) {}

    // helper
    fn visit_identifier_vector(&mut self, identifiers: &[Identifier]) {
        identifiers.iter().for_each(|i| self.visit_identifier(i));
    }

    fn enter_assignment(&mut self, _assignment: &Assignment) {}
    fn visit_assignment(&mut self, assignment: &Assignment) {
        self.enter_assignment(assignment);
        self.visit_identifier_vector(&assignment.variables);
        self.visit_expression(&assignment.value);
        self.exit_assignment(assignment);
    }
    fn exit_assignment(&mut self, _assignment: &Assignment) {}

    fn enter_expression(&mut self, _expression: &Expression) {}
    fn visit_expression(&mut self, expression: &Expression) {
        self.enter_expression(expression);
        match expression {
            Expression::Literal(literal) => self.visit_literal(literal),
            Expression::Identifier(identifier) => self.visit_identifier(identifier),
            Expression::FunctionCall(function) => self.visit_function_call(function),
        }
        self.exit_expression(expression);
    }
    fn exit_expression(&mut self, _expression: &Expression) {}

    fn enter_literal(&mut self, _literal: &Literal) {}
    fn visit_literal(&mut self, literal: &Literal) {
        self.enter_literal(literal);
        self.exit_literal(literal);
    }
    fn exit_literal(&mut self, _literal: &Literal) {}

    fn enter_identifier(&mut self, _identifier: &Identifier) {}
    fn visit_identifier(&mut self, identifier: &Identifier) {
        self.enter_identifier(identifier);
        self.exit_identifier(identifier);
    }
    fn exit_identifier(&mut self, _identifier: &Identifier) {}

    fn enter_function_call(&mut self, _fun_call: &FunctionCall) {}
    fn visit_function_call(&mut self, fun_call: &FunctionCall) {
        self.enter_function_call(fun_call);
        self.visit_identifier(&fun_call.function);
        fun_call
            .arguments
            .iter()
            .for_each(|i| self.visit_expression(i));
        self.exit_function_call(fun_call);
    }
    fn exit_function_call(&mut self, _fun_call: &FunctionCall) {}

    fn enter_if(&mut self, _x: &If) {}
    fn visit_if(&mut self, ifs: &If) {
        self.enter_if(ifs);
        self.visit_expression(&ifs.condition);
        self.visit_block(&ifs.body);
        self.exit_if(ifs);
    }
    fn exit_if(&mut self, _x: &If) {}

    fn enter_switch(&mut self, _x: &Switch) {}
    fn visit_switch(&mut self, switch: &Switch) {
        self.enter_switch(switch);
        self.visit_expression(&switch.expression);
        switch.cases.iter().for_each(|c| {
            if let Some(literal) = &c.literal {
                self.visit_literal(literal);
            }
            self.visit_block(&c.body);
        });
        self.exit_switch(switch);
    }
    fn exit_switch(&mut self, _x: &Switch) {}

    fn enter_for(&mut self, _x: &ForLoop) {}
    fn visit_for(&mut self, for_loop: &ForLoop) {
        self.enter_for(for_loop);
        self.visit_block(&for_loop.pre);
        self.visit_expression(&for_loop.condition);
        self.visit_block(&for_loop.post);
        self.visit_block(&for_loop.body);
        self.exit_for(for_loop);
    }
    fn exit_for(&mut self, _x: &ForLoop) {}

    fn enter_break(&mut self) {}
    fn visit_break(&mut self) {
        self.enter_break();
        self.exit_break();
    }
    fn exit_break(&mut self) {}

    fn enter_continue(&mut self) {}
    fn visit_continue(&mut self) {
        self.enter_continue();
        self.exit_continue();
    }
    fn exit_continue(&mut self) {}

    fn enter_leave(&mut self) {}
    fn visit_leave(&mut self) {
        self.enter_leave();
        self.exit_leave();
    }
    fn exit_leave(&mut self) {}
}

pub trait ASTModifier {
    fn enter_statement(&mut self, _st: &mut Statement) {}
    fn visit_statement(&mut self, st: &mut Statement) {
        self.enter_statement(st);
        match st {
            Statement::Block(block) => self.visit_block(block),
            Statement::FunctionDefinition(fun) => self.visit_function_definition(fun),
            Statement::VariableDeclaration(variable) => self.visit_variable_declaration(variable),
            Statement::Assignment(assignment) => self.visit_assignment(assignment),
            Statement::Expression(expression) => self.visit_expression(expression),
            Statement::If(if_st) => self.visit_if(if_st),
            Statement::Switch(switch) => self.visit_switch(switch),
            Statement::ForLoop(for_loop) => self.visit_for(for_loop),
            Statement::Break => self.visit_break(),
            Statement::Continue => self.visit_continue(),
            Statement::Leave => self.visit_leave(),
        };
        self.exit_statement(st);
    }
    fn exit_statement(&mut self, _st: &mut Statement) {}

    fn enter_block(&mut self, _block: &mut Block) {}
    fn visit_block(&mut self, block: &mut Block) {
        self.enter_block(block);
        block
            .statements
            .iter_mut()
            .for_each(|st| self.visit_statement(st));
        self.exit_block(block);
    }
    fn exit_block(&mut self, _block: &mut Block) {}

    fn enter_function_definition(&mut self, _fun_def: &mut FunctionDefinition) {}
    fn visit_function_definition(&mut self, fun_def: &mut FunctionDefinition) {
        self.enter_function_definition(fun_def);
        self.visit_identifier(&mut fun_def.name);
        self.visit_identifier_vector(&mut fun_def.parameters);
        self.visit_identifier_vector(&mut fun_def.returns);
        self.visit_block(&mut fun_def.body);
        self.exit_function_definition(fun_def);
    }
    fn exit_function_definition(&mut self, _fun_def: &mut FunctionDefinition) {}

    fn enter_variable_declaration(&mut self, _variable: &mut VariableDeclaration) {}
    fn visit_variable_declaration(&mut self, variable: &mut VariableDeclaration) {
        self.enter_variable_declaration(variable);
        self.visit_identifier_vector(&mut variable.variables);
        if let Some(value) = &mut variable.value {
            self.visit_expression(value);
        }
        self.exit_variable_declaration(variable);
    }
    fn exit_variable_declaration(&mut self, _variable: &mut VariableDeclaration) {}

    // helper
    fn visit_identifier_vector(&mut self, identifiers: &mut [Identifier]) {
        identifiers
            .iter_mut()
            .for_each(|i| self.visit_identifier(i));
    }

    fn enter_assignment(&mut self, _assignment: &mut Assignment) {}
    fn visit_assignment(&mut self, assignment: &mut Assignment) {
        self.enter_assignment(assignment);
        self.visit_identifier_vector(&mut assignment.variables);
        self.visit_expression(&mut assignment.value);
        self.exit_assignment(assignment);
    }
    fn exit_assignment(&mut self, _assignment: &mut Assignment) {}

    fn enter_expression(&mut self, _expression: &mut Expression) {}
    fn visit_expression(&mut self, expression: &mut Expression) {
        self.enter_expression(expression);
        match expression {
            Expression::Literal(literal) => self.visit_literal(literal),
            Expression::Identifier(identifier) => self.visit_identifier(identifier),
            Expression::FunctionCall(function) => self.visit_function_call(function),
        }
        self.exit_expression(expression);
    }
    fn exit_expression(&mut self, _expression: &mut Expression) {}

    fn enter_literal(&mut self, _literal: &mut Literal) {}
    fn visit_literal(&mut self, literal: &mut Literal) {
        self.enter_literal(literal);
        self.exit_literal(literal);
    }
    fn exit_literal(&mut self, _literal: &mut Literal) {}

    fn enter_identifier(&mut self, _identifier: &mut Identifier) {}
    fn visit_identifier(&mut self, identifier: &mut Identifier) {
        self.enter_identifier(identifier);
        self.exit_identifier(identifier);
    }
    fn exit_identifier(&mut self, _identifier: &mut Identifier) {}

    fn enter_function_call(&mut self, _fun_call: &mut FunctionCall) {}
    fn visit_function_call(&mut self, fun_call: &mut FunctionCall) {
        self.enter_function_call(fun_call);
        self.visit_identifier(&mut fun_call.function);
        fun_call
            .arguments
            .iter_mut()
            .for_each(|i| self.visit_expression(i));
        self.exit_function_call(fun_call);
    }
    fn exit_function_call(&mut self, _fun_call: &mut FunctionCall) {}

    fn enter_if(&mut self, _x: &mut If) {}
    fn visit_if(&mut self, ifs: &mut If) {
        self.enter_if(ifs);
        self.visit_expression(&mut ifs.condition);
        self.visit_block(&mut ifs.body);
        self.exit_if(ifs);
    }
    fn exit_if(&mut self, _x: &mut If) {}

    fn enter_switch(&mut self, _x: &mut Switch) {}
    fn visit_switch(&mut self, switch: &mut Switch) {
        self.enter_switch(switch);
        self.visit_expression(&mut switch.expression);
        switch.cases.iter_mut().for_each(|c| {
            if let Some(literal) = &mut c.literal {
                self.visit_literal(literal);
            }
            self.visit_block(&mut c.body);
        });
        self.exit_switch(switch);
    }
    fn exit_switch(&mut self, _x: &mut Switch) {}

    fn enter_for(&mut self, _x: &mut ForLoop) {}
    fn visit_for(&mut self, for_loop: &mut ForLoop) {
        self.enter_for(for_loop);
        self.visit_block(&mut for_loop.pre);
        self.visit_expression(&mut for_loop.condition);
        self.visit_block(&mut for_loop.post);
        self.visit_block(&mut for_loop.body);
        self.exit_for(for_loop);
    }
    fn exit_for(&mut self, _x: &mut ForLoop) {}

    fn enter_break(&mut self) {}
    fn visit_break(&mut self) {
        self.enter_break();
        self.exit_break();
    }
    fn exit_break(&mut self) {}

    fn enter_continue(&mut self) {}
    fn visit_continue(&mut self) {
        self.enter_continue();
        self.exit_continue();
    }
    fn exit_continue(&mut self) {}

    fn enter_leave(&mut self) {}
    fn visit_leave(&mut self) {
        self.enter_leave();
        self.exit_leave();
    }
    fn exit_leave(&mut self) {}
}
