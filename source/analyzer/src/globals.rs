use crate::EvaluatedType;

/// The path to the entry file.
/// This is only used in testing.
/// Its value in production is dependent on the environment.
pub const CORE_LIBRARY_PATH: &'static str = "/../corelib/Core/Core.wrl";

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
    Iteratable,
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
    const TRAITS: &'static str = "Prelude/Interfaces.wrl";
    const ITERATABLE: &'static str = "Prelude/Iteratable.wrl";
    const RANGE: &'static str = "Prelude/Range.wrl";
    const DEFAULT: &'static str = "Prelude/Default.wrl";
    const MAYBE: &'static str = "Prelude/Maybe.wrl";
}

pub const UNKNOWN: EvaluatedType = EvaluatedType::Unknown;
