use crate::EvaluatedType;

/// The path to the entry file.
/// This is only used in testing.
/// Its value in production is dependent on the environment.
pub const CORE_LIBRARY_PATH: &'static str = "../../library/core/core.wrl";

/// The maximum recursion limit for a type evaluation.
/// If leads to a stack overflow in Windows when its higher than 200, for some reason.
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
    Boolean,
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
    const STRING: &'static str = "string/string.wrl";
    const ARRAY: &'static str = "array/array.wrl";
    const BOOL: &'static str = "boolean/boolean.wrl";
    const NUMERIC: &'static str = "numeric/numeric.wrl";
    const CONCURRENT: &'static str = "concurrent/concurrent.wrl";
    const PROSPECT: &'static str = "concurrent/prospect.wrl";
    const EVENTUAL: &'static str = "concurrent/eventual.wrl";
    const INTERNAL: &'static str = "internals/internals.wrl";
    const NEVER: &'static str = "internals/never.wrl";
    const OPS: &'static str = "ops/ops.wrl";
    const INTERFACES: &'static str = "interfaces/interfaces.wrl";
    const RANGE: &'static str = "ops/range.wrl";
    const ITERATABLE: &'static str = "iterate/iterate.wrl";
    const TRY: &'static str = "interfaces/try.wrl";
    const GUARANTEED: &'static str = "interfaces/guaranteed.wrl";
    const DEFAULT: &'static str = "interfaces/default.wrl";
    const MAYBE: &'static str = "maybe/maybe.wrl";
}

pub const UNKNOWN: EvaluatedType = EvaluatedType::Unknown;
