/// This is a combination of Ok and Error types to produce a value that could exist, along with errors.
///
/// It is useful for operations that could induce multiple errors at once but still produce a possible outcome.
#[derive(Debug)]
pub struct Partial<T, E> {
    pub value: Option<T>,
    pub errors: Vec<E>,
}

impl<T, E> Partial<T, E> {
    /// Ignore all errors and return the result type.
    /// # Panics.
    /// This method will panic if any errors are encountered, or if there is no result.
    #[track_caller]
    pub fn unwrap(self) -> T {
        if self.errors.len() > 0 {
            panic!("Called Partial::unwrap() on a Partial with errors.")
        }
        match self.value {
            Some(result) => result,
            None => panic!("Called Partial::unwrap() on a Partial with no result."),
        }
    }
    /// Attempt to return the result type, or panic with a message based on the errors contained.
    #[track_caller]
    pub fn expect<F: FnOnce(Vec<E>) -> String>(self, callback_if_err: F) -> T {
        if self.errors.len() > 0 {
            panic!("{}", callback_if_err(self.errors))
        }
        match self.value {
            Some(result) => result,
            None => panic!(
                "Called Partial::unwrap() on a Partial with no result.\n {:}",
                callback_if_err(self.errors)
            ),
        }
    }
    /// Maps a `Partial<T, E>` to `Partial<U, E>` by applying a function to a contained value.
    /// The errors in the partial remain unchanged.
    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> Partial<U, E> {
        match self.value {
            Some(result) => Partial {
                value: Some(op(result)),
                errors: self.errors,
            },
            None => Partial {
                value: None,
                errors: self.errors,
            },
        }
    }
    /// Checks if a value exists.
    pub fn is_some(&self) -> bool {
        self.value.is_some()
    }
    /// Checks if a value does not exist,
    pub fn is_none(&self) -> bool {
        self.value.is_none()
    }
    /// Checks if any errors were encountered.
    pub fn has_errors(&self) -> bool {
        self.errors.len() > 0
    }
    /// Checks that a value exists and satisfies a preficate.
    pub fn exists_and<F: FnOnce(&T) -> bool>(&self, op: F) -> bool {
        if let Some(result) = &self.value {
            return op(result);
        }
        return false;
    }
    /// Construct a partial from a value type.
    pub fn from_value(value: T) -> Self {
        Self {
            value: Some(value),
            errors: vec![],
        }
    }
    /// Build a partial from one error.
    pub fn from_error(error: E) -> Self {
        Self {
            value: None,
            errors: vec![error],
        }
    }
    /// Build a partial from many errors.
    pub fn from_errors(errors: Vec<E>) -> Self {
        Self {
            value: None,
            errors,
        }
    }
    /// Build a partial from a tuple.
    pub fn from_tuple(tuple: (Option<T>, Vec<E>)) -> Self {
        Self {
            value: tuple.0,
            errors: tuple.1,
        }
    }
    /// Convert the partial into a tuple so the values can be handled separately.
    pub fn to_tuple(self) -> (Option<T>, Vec<E>) {
        (self.value, self.errors)
    }
    /// Add an error to the partial and returns it.
    pub fn with_error(mut self, error: E) -> Self {
        self.errors.push(error);
        self
    }
}

impl<T, E> From<Result<T, E>> for Partial<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(result) => Self {
                value: Some(result),
                errors: vec![],
            },
            Err(e) => Self {
                value: None,
                errors: vec![e],
            },
        }
    }
}
