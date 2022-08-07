pub trait Dialect {
    // TODO we actually should not need that.
    fn new() -> Self;
    fn is_builtin(name: &str) -> bool {
        Self::builtin(name).is_some()
    }
    fn builtin(name: &str) -> Option<Builtin>;
}

pub struct Builtin {
    name: String,
    parameters: u64,
    returns: u64,
}

pub struct EVMDialect {}

// macro_rules! b {
//     ($name:tt, $parameters:tt, $returns: tt) => {
//         name => Some(Builtin{name: $name, parameters: $parameters, returns: $returns}),
//     };
// }

fn b(name: &str, parameters: u64, returns: u64) -> Option<Builtin> {
    Some(Builtin {
        name: name.to_string(),
        parameters,
        returns,
    })
}
// TODO static hash map?
// TODO macro to avoid repetition?

impl Dialect for EVMDialect {
    fn new() -> Self {
        EVMDialect {}
    }
    fn builtin(name: &str) -> Option<Builtin> {
        match name {
            "stop" => b("stop", 0, 0),
            "add" => b("add", 2, 1),
            "sub" => b("sub", 2, 1),
            "mul" => b("mul", 2, 1),
            "div" => b("div", 2, 1),
            "sdiv" => b("sdiv", 2, 1),
            "mod" => b("mod", 2, 1),
            "smod" => b("smod", 2, 1),
            "exp" => b("exp", 2, 1),
            "not" => b("not", 1, 1),
            "lt" => b("lt", 2, 1),
            "gt" => b("gt", 2, 1),
            "slt" => b("slt", 2, 1),
            "sgt" => b("sgt", 2, 1),
            "eq" => b("eq", 2, 1),
            "iszero" => b("iszero", 1, 1),
            "and" => b("and", 2, 1),
            "or" => b("or", 2, 1),
            "xor" => b("xor", 2, 1),
            "byte" => b("byte", 2, 1),
            "shl" => b("shl", 2, 1),
            "shr" => b("shr", 2, 1),
            "sar" => b("sar", 2, 1),
            "addmod" => b("addmod", 3, 1),
            "mulmod" => b("mulmod", 3, 1),
            "signextend" => b("signextend", 2, 1),
            "keccak256" => b("keccak256", 2, 1),
            "address" => b("address", 0, 1),
            "balance" => b("balance", 1, 1),
            "origin" => b("origin", 0, 1),
            "caller" => b("caller", 0, 1),
            "callvalue" => b("callvalue", 0, 1),
            "calldataload" => b("calldataload", 1, 1),
            "calldatasize" => b("calldatasize", 0, 1),
            "calldatacopy" => b("calldatacopy", 3, 0),
            "codesize" => b("codesize", 0, 1),
            "codecopy" => b("codecopy", 3, 0),
            "gasprice" => b("gasprice", 0, 1),
            "extcodesize" => b("extcodesize", 1, 1),
            "extcodecopy" => b("extcodecopy", 4, 0),
            "returndatasize" => b("returndatasize", 0, 1),
            "returndatacopy" => b("returndatacopy", 3, 0),
            "extcodehash" => b("extcodehash", 1, 1),
            "blockhash" => b("blockhash", 1, 1),
            "coinbase" => b("coinbase", 0, 1),
            "timestamp" => b("timestamp", 0, 1),
            "number" => b("number", 0, 1),
            "difficulty" => b("difficulty", 0, 1),
            "gaslimit" => b("gaslimit", 0, 1),
            "chainid" => b("chainid", 0, 1),
            "selfbalance" => b("selfbalance", 0, 1),
            "basefee" => b("basefee", 0, 1),
            "pop" => b("pop", 1, 0),
            "mload" => b("mload", 1, 1),
            "mstore" => b("mstore", 2, 0),
            "mstore8" => b("mstore8", 2, 0),
            "sload" => b("sload", 1, 1),
            "sstore" => b("sstore", 2, 0),
            // "jump" => b("jump", 1, 0),
            // "jumpi" => b("jumpi", 2, 0),
            // "pc" => b("pc", 0, 1),
            "msize" => b("msize", 0, 1),
            "gas" => b("gas", 0, 1),
            "log0" => b("log0", 2, 0),
            "log1" => b("log1", 3, 0),
            "log2" => b("log2", 4, 0),
            "log3" => b("log3", 5, 0),
            "log4" => b("log4", 6, 0),
            "create" => b("create", 3, 1),
            "call" => b("call", 7, 1),
            "callcode" => b("callcode", 7, 1),
            "return" => b("return", 2, 0),
            "delegatecall" => b("delegatecall", 6, 1),
            "staticcall" => b("staticcall", 6, 1),
            "create2" => b("create2", 4, 1),
            "revert" => b("revert", 2, 0),
            "invalid" => b("invalid", 0, 0),
            "selfdestruct" => b("selfdestruct", 1, 0),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn some_basics() {
        assert!(EVMDialect::is_builtin("add"));
        assert!(!EVMDialect::is_builtin("dup"));
        assert!(!EVMDialect::is_builtin("dup1"));
        assert!(!EVMDialect::is_builtin("swap1"));
        assert!(!EVMDialect::is_builtin("jump"));
        assert!(!EVMDialect::is_builtin("jumpi"));
        assert!(!EVMDialect::is_builtin("pc"));
    }
}
