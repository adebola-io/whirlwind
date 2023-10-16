use std::fmt::Display;

use tower_lsp::lsp_types::MessageType;

/// A list of messages to be logged to the server's client whenever the async shenanigans allows it.
#[derive(Default)]
pub struct MessageStore {
    messages: Vec<(MessageType, String)>,
}

pub type WithMessages<T> = (MessageStore, T);

impl MessageStore {
    /// Creates a new message store.
    pub fn new() -> Self {
        Self { messages: vec![] }
    }

    /// Adds a netral message to the store.
    pub fn inform<T: Display>(&mut self, message: T) {
        self.messages.push((MessageType::INFO, message.to_string()))
    }

    /// Adds an error to the store.
    pub fn error<T: Display>(&mut self, message: T) {
        self.messages
            .push((MessageType::ERROR, message.to_string()))
    }
}

impl IntoIterator for MessageStore {
    type Item = (MessageType, String);

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.messages.into_iter()
    }
}
