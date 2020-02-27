use crate::yul;

/// Creates a Yul literal.
#[macro_export]
macro_rules! literal {
    {[$e:expr]} => {$e};
    {$l:literal} => {yul::Literal { literal: stringify!($l).to_string(), yultype: None }};
}

/// Creates a Yul literal expression.
#[macro_export]
macro_rules! literal_expression {
    {$($literal:tt)*} => {yul::Expression::Literal(literal!($($literal)*))};
}

/// Creates a Yul identifier.
#[macro_export]
macro_rules! identifier {
    {[$e:expr]} => {$e};
    {$i:ident} => {yul::Identifier { identifier: stringify!($i).to_string(), yultype: None }};
}

/// Creates a vec of Yul identifiers.
#[macro_export]
macro_rules! identifiers {
    {@as_vec [$identifiers:tt...]} => { $identifiers.clone() };
    {@as_vec $($identifier:tt)*} => {vec![identifier! {$($identifier)*}]};

    {$($identifiers:tt)*} => {{
        let mut identifiers = vec![];
        $(identifiers.append(&mut identifiers! {@as_vec $identifiers});)*
        identifiers
    }};
}

/// Creates a Yul identifier expression.
#[macro_export]
macro_rules! identifier_expression {
    {$($identifier:tt)*} => {yul::Expression::Identifier(identifier! {$($identifier)*})};
}

/// Creates a Yul function call.
#[macro_export]
macro_rules! function_call {
    {[$e:expr]} => {$e};
    {$name:tt($($args:tt),*)} => {
        yul::FunctionCall {
            identifier: identifier! {$name},
            arguments: expressions! {$($args)*}
        }
    };
}

/// Creates a function call expression.
#[macro_export]
macro_rules! function_call_expression {
    {$($function_call:tt)*} => {
        yul::Expression::FunctionCall(function_call! {$($function_call)*})
    };
}

/// Creates a function call statement.
#[macro_export]
macro_rules! function_call_statement {
    {$($function_call:tt)*} => {
        yul::Statement::Expression(function_call_expression! {$($function_call)*})
    };
}

/// Creates a Yul expression.
#[macro_export]
macro_rules! expression {
    {[$e:expr]} => {$e};
    {($($expression:tt)*)} => {expression! {$($expression)*}};
    {$name:tt($($args:tt)*)} => {function_call_expression! {$name($($args)*)}};
    {$l:literal} => {literal_expression! {$l}};
    {$i:ident} => {identifier_expression! {$i}};
}

/// Creates a vec of Yul expressions.
#[macro_export]
macro_rules! expressions {
     {$($expressions:tt)*} => {{
        let mut expressions = vec![];
        $(expressions.push(expression! {$expressions});)*
        expressions
    }};
}

/// Creates a Yul variable declaration statement.
#[macro_export]
macro_rules! variable_declaration {
    {let $name:tt := $($expression:tt)*} => {
        yul::Statement::VariableDeclaration(yul::VariableDeclaration {
            identifiers: vec![identifier! {$name}],
            expression: Some(expression! {$($expression)*})
        })
    };
}

/// Creates a Yul assignment statement.
#[macro_export]
macro_rules! assignment {
    {$name:tt := $($expression:tt)+} => {
        yul::Statement::Assignment(yul::Assignment {
            identifiers: vec![identifier! {$name}],
            expression: expression! {$($expression)*}
        })
    };
}

/// Creates a Yul statement.
#[macro_export]
macro_rules! statement {
    {[$e:expr]} => {$e};
    {($($statement:tt)*)} => {statement! {$($statement)*}};
    {$name:tt($($args:tt)*)} => {function_call_statement! {$name($($args)*)}};
    {let $name:tt := $($expression:tt)*} => {variable_declaration! {let $name := $($expression)*}};
    {$name:tt := $($expression:tt)*} => {assignment! {$name := $($expression)*}};
    {function $name:tt($($params:tt),*) $(-> $returns:tt)? {$($statements:tt)*}} => {
        function $name($($params),*) $(-> $returns)? {$($statements)*}
    }
}

/// Creates a vec of Yul statements.
#[macro_export]
macro_rules! statements {
    {@as_vec [$statements:tt...]} => { $statements.clone() };
    {@as_vec $($statement:tt)*} => {vec![statement! {$($statement)*}]};

    {$($statement:tt)*} => {{
       let mut statements = vec![];
       $(statements.append(&mut statements! {@as_vec $statement});)*
       statements
    }};
}

/// Creates a Yul block.
#[macro_export]
macro_rules! block {
    {[$e:expr]} => {$e};
    {$($statements:tt)*} => { yul::Block { statements: statements! {$($statements)*} }};
}

/// Creates a Yul function definition.
#[macro_export]
macro_rules! function_definition {
    {function $name:tt($($params:tt),*) $(-> $returns:tt)? {$($statements:tt)*}} => {
        yul::Statement::FunctionDefinition(yul::FunctionDefinition {
            name: identifier! {$name},
            parameters: identifiers! {$($params)*},
            returns: {
                let mut returns = vec![];
                $(returns.push(identifier!{$returns});)*
                returns
            },
            block: block!{$($statements)*},
        })
    };
}

/// Creates a Yul switch statement.
#[macro_export]
macro_rules! switch {
    {@case (case $literal:tt { $($statements:tt)* })} => {
        yul::Case {
            literal: Some(literal! {$literal}),
            block: block! {$($statements)*}
        }
    };
    {@case (default { $($statements:tt)* })} => {
        yul::Case {
            literal: None,
            block: block! {$($statements)*}
        }
    };

    {switch $expression:tt $($cases:tt)*} => {
        yul::Statement::Switch(yul::Switch {
            expression: expression! {$expression},
            cases: {
                let mut cases = vec![];
                $(cases.push(switch! {@case $cases});)*
                cases
            }
        })
    };
}

#[cfg(test)]
mod tests {
    use crate::yul;

    #[test]
    fn literal_string() {
        assert_eq!(literal! {"foo"}.to_string(), r#""foo""#)
    }

    #[test]
    fn literal_num() {
        assert_eq!(literal! {42}.to_string(), "42")
    }

    #[test]
    fn literal_node() {
        let foo = yul::Literal {
            literal: r#""bar""#.to_string(),
            yultype: None,
        };
        assert_eq!(literal! {[foo]}.to_string(), r#""bar""#)
    }

    #[test]
    fn literal_expression_string() {
        assert_eq!(literal_expression! {"foo"}.to_string(), r#""foo""#)
    }

    #[test]
    fn expression_literal() {
        assert_eq!(expression! {"foo"}.to_string(), r#""foo""#)
    }

    #[test]
    fn expression_node() {
        let node = literal_expression!("foobar");

        assert_eq!(expression! {[node]}.to_string(), r#""foobar""#)
    }

    #[test]
    fn expression_identifier() {
        assert_eq!(expression! {foo}.to_string(), "foo")
    }

    #[test]
    fn function_call() {
        assert_eq!(
            function_call! {foo("string", bar, 42)}.to_string(),
            r#"foo("string", bar, 42)"#
        )
    }

    #[test]
    fn function_call_node() {
        let node = literal_expression!("foobar");

        assert_eq!(
            function_call! {foo("string", bar, 42, [node])}.to_string(),
            r#"foo("string", bar, 42, "foobar")"#
        )
    }

    #[test]
    fn function_call_expression() {
        assert_eq!(
            function_call_expression! {foo("string", bar, 42)}.to_string(),
            r#"foo("string", bar, 42)"#
        )
    }

    #[test]
    fn function_call_statement() {
        assert_eq!(
            function_call_statement! {foo("string", bar, 42)}.to_string(),
            r#"foo("string", bar, 42)"#
        )
    }

    #[test]
    fn expression_function_call() {
        let node = literal_expression!("foobar");

        assert_eq!(
            expression! {foo("string", bar, 42, [node])}.to_string(),
            r#"foo("string", bar, 42, "foobar")"#
        )
    }

    #[test]
    fn variable_declaration() {
        assert_eq!(
            variable_declaration! {let foo := 42}.to_string(),
            "let foo := 42"
        )
    }

    #[test]
    fn variable_declaration_function() {
        assert_eq!(
            variable_declaration! {let foo := foo("bar", 42)}.to_string(),
            r#"let foo := foo("bar", 42)"#
        )
    }

    #[test]
    fn variable_declaration_function_nested_node() {
        let food = function_call_expression!(food("taco", apple));

        assert_eq!(
            variable_declaration! {let foo := foo("bar", [food])}.to_string(),
            r#"let foo := foo("bar", food("taco", apple))"#
        )
    }

    #[test]
    fn variable_declaration_identifier_node() {
        let foo = identifier!(foo);

        assert_eq!(
            variable_declaration! {let [foo] := bar("baz")}.to_string(),
            r#"let foo := bar("baz")"#
        )
    }

    #[test]
    fn variable_declaration_function_nested_raw() {
        assert_eq!(
            variable_declaration! {let foo := foo("bar", (food("taco", apple)))}.to_string(),
            r#"let foo := foo("bar", food("taco", apple))"#
        )
    }

    #[test]
    fn assignment() {
        assert_eq!(assignment! {foo := 42}.to_string(), "foo := 42")
    }

    #[test]
    fn statement_function() {
        let _42 = expression! {42};
        let biz = function_call_expression! {biz(bit, coin, [_42])};
        assert_eq!(
            statement! {
                bar(
                    "ding",
                     dong,
                     [biz],
                     (farm(cow, "sheep"))
                )
            }
                .to_string(),
            r#"bar("ding", dong, biz(bit, coin, 42), farm(cow, "sheep"))"#
        )
    }

    #[test]
    fn statement_variable_declaration() {
        assert_eq!(
            statement! {let foo := bar("ding", dong)}.to_string(),
            r#"let foo := bar("ding", dong)"#
        )
    }

    #[test]
    fn statement_assignment() {
        assert_eq!(statement! {foo := 42}.to_string(), "foo := 42")
    }

    #[test]
    fn block() {
        assert_eq!(
            block! {
                (let foo := 42)
                (bar(foo))
            }
                .to_string(),
            "{ let foo := 42 bar(foo) }"
        )
    }

    #[test]
    fn block_multi_insert() {
        let statements = vec![
            statement! { let bar := 2 },
            statement! { blockchain(_3d) },
        ];

        assert_eq!(
            block! {
                (let foo := 42)
                [statements...]
                (bar(foo))
            }
                .to_string(),
            "{ let foo := 42 let bar := 2 blockchain(_3d) bar(foo) }"
        )
    }

    #[test]
    fn function_definition() {
        let bit = identifier! {bit};

        assert_eq!(
            function_definition! {
                function foo([bit], coin) -> bar {
                    (let baz := add(bit, coin))
                    (bar := hello_world(baz, "hi"))
                }
            }
                .to_string(),
            r#"function foo(bit, coin) -> bar { let baz := add(bit, coin) bar := hello_world(baz, "hi") }"#
        )
    }

    #[test]
    fn function_definition_no_return() {
        let bit = identifier! {bit};

        assert_eq!(
            function_definition! {
                function foo([bit], [identifier! {coin}]) {
                    (let baz := add(bit, coin))
                    (bar := hello_world(baz, "hi"))
                }
            }.to_string(),
            r#"function foo(bit, coin) { let baz := add(bit, coin) bar := hello_world(baz, "hi") }"#
        )
    }

    #[test]
    fn switch() {
        let foo = expression! {foo(1, "s")};
        let bing = expression! {bing("bong")};
        let _42 = literal! {42};

        assert_eq! {
            switch! {
                switch [foo]
                (case 1 {
                    (bar(42))
                })
                (case [_42] {
                    (bar(420))
                    (baz("block", chain))
                })
                (default {
                    (let bing := [bing])
                    (bar(bing))
                })
            }.to_string(),
            r#"switch foo(1, "s") case 1 { bar(42) } case 42 { bar(420) baz("block", chain) } default { let bing := bing("bong") bar(bing) } "#
        }
        #[test]

        fn switch_no_default() {
            let foo = expression! {foo(1, "s")};
            let bing = expression! {bing("bong")};
            let _42 = literal! {42};

            assert_eq! {
                switch! {
                switch [foo]
                (case 1 {
                    (bar(42))
                })
                (case [_42] {
                    (bar(420))
                    (baz("block", chain))
                })
            }.to_string(),
                r#"switch foo(1, "s") case 1 { bar(42) } case 42 { bar(420) baz("block", chain) } "#
            }
        }   }

    #[test]
    fn statements() {
        let good_statements = statements! {
            (let a := my_function("hello", (world("42"))))
            (b := add(a, 2))
        };

        let food = identifier! {food};
        let better_statements = statements! {
            (let value := [food](1, 2))
            (let another_value := 42)
        };

        let best_statement = statement! { foo := "42 bar 42" };

        assert_eq!(
            statements! {
                (let kung := foo)
                [good_statements...]
                (ip := man())
                [better_statements...]
                [best_statement]
            }.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(" "),
            r#"let kung := foo let a := my_function("hello", world("42")) b := add(a, 2) ip := man() let value := food(1, 2) let another_value := 42 foo := "42 bar 42""#
        )
    }

    #[test]
    fn object_w_function_definitions() {
        let one = identifier! {one};
        let foo_idents = identifiers! {test [one] two};
        let foo_func = function_definition! {
            function foo([foo_idents...]) {
                (log("hello_world"))
            }
        };

        let bar_idents = identifiers! {three four};
        let bar_func = function_definition! {
            function foo(two, [bar_idents...]) -> return_val {
                (let a := test(two, three, four))
                (return_val := a)
            }
        };

        assert_eq!(
            block! {
                (let a := b)
                [foo_func]
                [bar_func]
            }.to_string(),
            r#"{ let a := b function foo(test, one, two) { log("hello_world") } function foo(two, three, four) -> return_val { let a := test(two, three, four) return_val := a } }"#
        )
    }
}