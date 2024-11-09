use analyzer::TypedFunctionDeclaration;

const REPORT: &str =
    "This is a compiler bug. Please report this at https://github.com/adebola-io/whirlwind/issues.";
pub fn function_mismatch_error(function_symbol: &analyzer::SemanticSymbol) {
    unreachable!("A function declaration was matched to a symbol of type {:?} during bytecode generation. {REPORT}", function_symbol.kind)
}

pub fn function_resolve_error(function_symbol: &TypedFunctionDeclaration) -> ! {
    unreachable!("A function declaration {:?} did not resolve to a symbol during bytecode generation. {REPORT}", function_symbol)
}
