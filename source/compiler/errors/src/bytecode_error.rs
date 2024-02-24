#[derive(Debug)]
pub enum BytecodeError {
    MainIsAsync,
    MainReturns,
    MainNotFound,
    MainHasParameters,
}
