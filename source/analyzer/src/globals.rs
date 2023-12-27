use crate::EvaluatedType;

/// The path to the entry file.
/// This is only used in testing.
/// Its value in production is dependent on the environment.
pub const CORE_LIBRARY_PATH: &'static str = "../corelib/core/core.wrl";

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
    Concurrent,
    Eventual,
    Prospect,
    Internal,
    Ops,
    Interfaces,
    Guaranteed,
    Try,
    Iteration,
    Range,
    Default,
    Maybe,
    Never,
}
pub trait IntrinsicPaths {
    const PRELUDE: &'static str = "prelude/prelude.wrl";
    const STRING: &'static str = "primitives/string.wrl";
    const ARRAY: &'static str = "primitives/array.wrl";
    const BOOL: &'static str = "primitives/bool.wrl";
    const NUMERIC: &'static str = "primitives/numeric.wrl";
    const CONCURRENT: &'static str = "concurrent/concurrent.wrl";
    const PROSPECT: &'static str = "concurrent/prospect.wrl";
    const EVENTUAL: &'static str = "concurrent/eventual.wrl";
    const INTERNAL: &'static str = "internals/internals.wrl";
    const NEVER: &'static str = "internals/never.wrl";
    const OPS: &'static str = "primitives/ops.wrl";
    const INTERFACES: &'static str = "prelude/interfaces.wrl";
    const RANGE: &'static str = "prelude/range.wrl";
    const ITERATABLE: &'static str = "prelude/interfaces/iteration.wrl";
    const TRY: &'static str = "prelude/interfaces/try.wrl";
    const GUARANTEED: &'static str = "prelude/interfaces/guaranteed.wrl";
    const DEFAULT: &'static str = "prelude/interfaces/default.wrl";
    const MAYBE: &'static str = "prelude/maybe.wrl";
}

pub const UNKNOWN: EvaluatedType = EvaluatedType::Unknown;
