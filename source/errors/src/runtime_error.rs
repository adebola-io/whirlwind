#[derive(Debug, Clone, Copy)]
pub enum ExecutionError {
    MainCrashed,
    MainFunctionNotDefined,
    StackOverflow,
    IllegalMemoryAccess,
}
