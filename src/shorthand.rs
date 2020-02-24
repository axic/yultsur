use crate::yul;

/// Creates a Yul literal.
macro_rules! literal {
    {[$i:ident]} => {$i};
    {$l:literal} => {yul::Literal { literal: stringify!($l).to_string(), yultype: None }};
}

/// Creates a Yul literal expression.
macro_rules! literal_expression {
    {$($tts:tt)*} => {yul::Expression::Literal(literal!($($tts)*))};
}

/// Creates a Yul identifier.
macro_rules! identifier {
    {[$i:ident]} => {$i};
    {$l:ident} => {yul::Identifier { identifier: stringify!($l).to_string(), yultype: None }};
}

/// Creates a Yul identifier expression.
macro_rules! identifier_expression {
    {$($tts:tt)*} => {yul::Expression::Identifier(identifier!($($tts)*))};
}

/// Creates a Yul function call.
macro_rules! function_call {
    {[$i:ident]} => {$i};
    {$name:ident($($arg:tt),*)} => {
        yul::FunctionCall {
            identifier: identifier!{$name},
            arguments: {
                let mut args = vec![];
                $(
                    args.push(expression!{$arg});
                )*
                args
            }
        }
    };
}

/// Creates a function call expression.
macro_rules! function_call_expression {
    {$($tts:tt)*} => {yul::Expression::FunctionCall(function_call!($($tts)*))};
}

/// Creates a function call statement.
macro_rules! function_call_statement {
    {$($tts:tt)*} => {yul::Statement::Expression(
        yul::Expression::FunctionCall(function_call!($($tts)*))
    )};
}

/// Creates a Yul expression.
macro_rules! expression {
    {[$i:ident]} => {$i};
    {$l:literal} => {literal_expression!{$l}};
    {$i:ident} => {identifier_expression!{$i}};
    {$($tts:tt)*} => {function_call_expression!($($tts)*)};
}

/// Creates a Yul variable declaration statement.
macro_rules! variable_declaration {
    {let $name:ident := $($tts:tt)+} => {
        yul::Statement::VariableDeclaration(yul::VariableDeclaration {
            identifiers: vec![identifier!{$name}],
            expression: Some(expression!{$($tts)*})
        })
    };
}

/// Creates a Yul assignment statement.
macro_rules! assignment {
    {$name:ident := $($tts:tt)+} => {
        yul::Statement::Assignment(yul::Assignment {
            identifiers: vec![identifier!{$name}],
            expression: expression!{$($tts)*}
        })
    };
}

#[cfg(test)]
mod tests {
    use crate::yul;

    #[test]
    fn test_literal_string () {
        assert_eq!(
            literal!{"foo"}.to_string(),
            r#""foo""#
        )
    }

    #[test]
    fn test_literal_num () {
        assert_eq!(
            literal!{42}.to_string(),
            "42"
        )
    }

    #[test]
    fn test_literal_node () {
        let foo = yul::Literal { literal: r#""bar""#.to_string(), yultype: None };
        assert_eq!(
            literal!{[foo]}.to_string(),
            r#""bar""#
        )
    }

    #[test]
    fn test_literal_expression_string () {
        assert_eq!(
            literal_expression!{"foo"}.to_string(),
            r#""foo""#
        )
    }

    #[test]
    fn test_expression_literal () {
        assert_eq!(
            expression!{"foo"}.to_string(),
            r#""foo""#
        )
    }

    #[test]
    fn test_expression_node () {
        let node = literal_expression!("foobar");

        assert_eq!(
            expression!{[node]}.to_string(),
            r#""foobar""#
        )
    }

    #[test]
    fn test_expression_identifier () {
        assert_eq!(
            expression!{foo}.to_string(),
            "foo"
        )
    }

    #[test]
    fn test_function_call () {
        assert_eq!(
            function_call!{foo("string", bar, 42)}.to_string(),
            r#"foo("string", bar, 42)"#
        )
    }

   #[test]
    fn test_function_call_node () {
        let node = literal_expression!("foobar");

        assert_eq!(
            function_call!{foo("string", bar, 42, [node])}.to_string(),
            r#"foo("string", bar, 42, "foobar")"#
        )
    }

    #[test]
    fn test_function_call_expression () {
        assert_eq!(
            function_call_expression!{foo("string", bar, 42)}.to_string(),
            r#"foo("string", bar, 42)"#
        )
    }

    #[test]
    fn test_function_call_statement () {
        assert_eq!(
            function_call_statement!{foo("string", bar, 42)}.to_string(),
            r#"foo("string", bar, 42)"#
        )
    }

    #[test]
    fn test_expression_function_call () {
        let node = literal_expression!("foobar");

        assert_eq!(
            expression!{foo("string", bar, 42, [node])}.to_string(),
            r#"foo("string", bar, 42, "foobar")"#
        )
    }

    #[test]
    fn test_variable_declaration () {
        assert_eq!(
            variable_declaration!{let foo := 42}.to_string(),
            "let foo := 42"
        )
    }

    #[test]
    fn test_variable_declaration_function () {
        assert_eq!(
            variable_declaration!{let foo := foo("bar", 42)}.to_string(),
            r#"let foo := foo("bar", 42)"#
        )
    }

    #[test]
    fn test_variable_declaration_function_nested_node () {
        let food = function_call_expression!(food("taco", apple));

        assert_eq!(
            variable_declaration!{let foo := foo("bar", [food])}.to_string(),
            r#"let foo := foo("bar", food("taco", apple))"#
        )
    }

    #[test]
    fn test_assignment () {
        assert_eq!(
            assignment!{foo := 42}.to_string(),
            "foo := 42"
        )
    }
}
