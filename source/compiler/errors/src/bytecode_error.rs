#[derive(Debug)]
pub enum BytecodeError {
    /// The main function is marked as asynchronous.
    MainIsAsync,
    /// The main function has a return type.
    MainReturns,
    /// The main function could not be found in the symbol table.
    MainNotFound,
    /// The main function has parameters.
    MainHasParameters,
    /// The symbol for the main function could not be reolved from the symbol table.
    MainNotResolvable,
    /// The main function is not marked as public.
    MainNotPublic,
    /// The main function is imported from an external library.
    MainIsImported,
}
