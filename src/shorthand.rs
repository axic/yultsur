use crate::yul;

/// Creates a Yul literal.
#[macro_export]
macro_rules! literal {
    {[$e:expr]} => {$e};
    {($e:expr)} => {yul::Literal { literal: $e.to_string(), yultype: None }};
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
    {($e:expr)} => {yul::Identifier { identifier: $e.to_string(), yultype: None }};
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
     {@as_vec [$expressions:tt...]} => { $expressions.clone() };
     {@as_vec $($expression:tt)*} => {vec![expression! {$($expression)*}]};

     {$($expressions:tt)*} => {{
         let mut expressions = vec![];
         $(expressions.append(&mut expressions! {@as_vec $expressions});)*
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
    };
    {switch $expression:tt $($cases:tt)*} => {switch! {switch $expression $($cases)*}};
    {if $expression:tt { $($block:tt)* }} => {_if! {if $expression { $($block)* }}};
    {for { $($pre:tt)* } $condition:tt { $($post:tt)* } { $($body:tt)* } } => {
        for_loop! { for { $($pre)* } $condition { $($post)* } { $($body)* } }
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

/// Create a Yul case.
#[macro_export]
macro_rules! case {
    {[$e:expr]} => {$e};
    {($($case:tt)*)} => {case! {$($case)*}};
    {case $literal:tt { $($statements:tt)* }} => {
        yul::Case {
            literal: Some(literal! {$literal}),
            block: block! {$($statements)*}
        }
    };
    {default { $($statements:tt)* }} => {
        yul::Case {
            literal: None,
            block: block! {$($statements)*}
        }
    };
}

/// Creates a vec of Yul cases.
#[macro_export]
macro_rules! cases {
    {@as_vec [$cases:tt...]} => { $cases.clone() };
    {@as_vec $($case:tt)*} => {vec![case! {$($case)*}]};

    {$($case:tt)*} => {{
       let mut cases = vec![];
       $(cases.append(&mut cases! {@as_vec $case});)*
       cases
    }};
}

/// Creates a Yul switch statement.
#[macro_export]
macro_rules! switch {
    {switch $expression:tt $($cases:tt)*} => {
        yul::Statement::Switch(yul::Switch {
            expression: expression! {$expression},
            cases: cases! {$($cases)*}
        })
    };
}

/// Creates a Yul if statement
#[macro_export]
macro_rules! _if {
    {if $expression:tt { $($block:tt)* }} => {
        yul::Statement::If(yul::If {
            expression: expression! {$expression},
            block: block! {$($block)*}
        })
    }
}

/// Creates a Yul for loop statement
#[macro_export]
macro_rules! for_loop {
    {for { $($pre:tt)* } $condition:tt { $($post:tt)* } { $($body:tt)* } } => {
        yul::Statement::ForLoop(yul::ForLoop {
            pre: block! {$($pre)*},
            condition: expression! {$condition},
            post: block! {$($post)*},
            body: block! {$($body)*}
        })
    }
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
        let expressions = expressions! { bar "foo" (call()) };

        assert_eq!(
            function_call! { foo("string", bar, 42, [expressions...]) }.to_string(),
            r#"foo("string", bar, 42, bar, "foo", call())"#
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
                (if 0 { (call(42)) })
            }.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(" "),
            r#"let kung := foo let a := my_function("hello", world("42")) b := add(a, 2) ip := man() let value := food(1, 2) let another_value := 42 foo := "42 bar 42" if 0 { call(42) }"#
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

    #[test]
    fn object_w_switch() {
        assert_eq!(
            block! {
                (let a := 40)
                (let b := 2)
                (switch (add(a,b))
                    (case 42 {
                        (let c := 2)
                    })
                    (case "3d" {
                        (foo(0))
                        (bar("test"))
                    })
                )
            }.to_string(),
            r#"{ let a := 40 let b := 2 switch add(a, b) case 42 { let c := 2 } case "3d" { foo(0) bar("test") }  }"#
        )
    }

    #[test]
    fn cases_in_switch() {
        let case_foo = case! {
            case "foo" { (test(42)) }
        };

        let case_bar = case! {
            case "bar" { (hello_world(42)) (a := b) }
        };

        let cases = vec![case_foo, case_bar];

        let default = case! {
            default {
                (c := 4)
            }
        };

        assert_eq!(
            switch! {
                switch (cat("f",s))
                [cases...]
                [default]
            }.to_string(),
            r#"switch cat("f", s) case "foo" { test(42) } case "bar" { hello_world(42) a := b } default { c := 4 } "#
        )
    }

    #[test]
    fn _if() {
        assert_eq!(
            _if! { if (eq(foo, 0)) { (let a := b) } }.to_string(),
            "if eq(foo, 0) { let a := b }"
        )
    }

    #[test]
    fn for_loop() {
        assert_eq!(
            for_loop! {
                for { (let i := 0) } (lt(i, exponent)) { (i := add(i, 1)) }
                {
                    (result := mul(result, base))
                }
            }.to_string(),
            "for { let i := 0 } lt(i, exponent) { i := add(i, 1) } { result := mul(result, base) }"
        )
    }

    #[test]
    fn statement_for_loop() {
        assert_eq!(
            statement! {
                for { (let i := 0) } (lt(i, exponent)) { (i := add(i, 1)) }
                {
                    (result := mul(result, base))
                }
            }.to_string(),
            "for { let i := 0 } lt(i, exponent) { i := add(i, 1) } { result := mul(result, base) }"
        )
    }

    #[test]
    fn identifier_from_expression() {
        let identifier = "test";

        assert_eq!(
            identifier! {(identifier)}.to_string(),
            "test"
        );

        assert_eq!(
            identifier_expression! {(identifier)}.to_string(),
            "test"
        )
    }

    #[test]
    fn literal_from_expression() {
        assert_eq!(
            literal! {(1 + 1)}.to_string(),
            "2"
        );

        let foo = r#""bar""#;
        assert_eq!(
            literal_expression! {(foo)}.to_string(),
            r#""bar""#
        )
    }
}