#![allow(unused)]
use std::{io::Error as IOError, path::PathBuf};
use url::{ParseError, Url};

#[derive(Debug)]
pub enum DocumentError {
    IOError(IOError),
    NoParentFolder(PathBuf),
    WhattheHellHappened,
    CouldNotConvert(Url),
    CouldNotDetermineMain,   
}

impl From<IOError> for DocumentError {
    fn from(value: IOError) -> Self {
        DocumentError::IOError(value)
    }
}
