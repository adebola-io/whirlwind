use crate::EvaluatedType;

/// The path to the entry file.
/// This is only used in testing.
/// Its value in production is dependent on the environment.
pub const CORE_LIBRARY_PATH: &'static str = "../corelib/Core/Core.wrl";

/// The maximum recursion limit for a type evaluation.
pub const EVALUATION_DEPTH: u64 = 200;

/// The number of related symbols to show in the hover of a symbol.
pub const RELATION_COUNT: u8 = 5;

/// Signifies the file being parsed.
/// Useful to pinpoint primitive types without too much build-in into
/// the compiler.
pub enum CurrentModuleType {
    Regular,
    String,
    Array,
    Bool,
    Numeric,
    Async,
    Internal,
    Ops,
    Interfaces,
    Guaranteed,
    Try,
    Iteration,
    Range,
    Default,
    Maybe,
}
pub trait IntrinsicPaths {
    const PRELUDE: &'static str = "Prelude/Prelude.wrl";
    const STRING: &'static str = "Primitives/String.wrl";
    const ARRAY: &'static str = "Primitives/Array.wrl";
    const BOOL: &'static str = "Primitives/Bool.wrl";
    const NUMERIC: &'static str = "Primitives/Numeric.wrl";
    const ASYNC: &'static str = "Async/Async.wrl";
    const INTERNAL: &'static str = "Internals/Internals.wrl";
    const OPS: &'static str = "Primitives/Ops.wrl";
    const INTERFACES: &'static str = "Prelude/Interfaces.wrl";
    const RANGE: &'static str = "Prelude/Range.wrl";
    const ITERATABLE: &'static str = "Prelude/Interfaces/Iteration.wrl";
    const TRY: &'static str = "Prelude/Interfaces/Try.wrl";
    const GUARANTEED: &'static str = "Prelude/Interfaces/Guaranteed.wrl";
    const DEFAULT: &'static str = "Prelude/Interfaces/Default.wrl";
    const MAYBE: &'static str = "Prelude/Maybe.wrl";
}

pub const UNKNOWN: EvaluatedType = EvaluatedType::Unknown;
