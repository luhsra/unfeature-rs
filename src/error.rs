use std::{fmt, io};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    CargoMetadata(cargo_metadata::Error),
    Syn(syn::Error),
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(e) => fmt::Display::fmt(e, f),
            Error::CargoMetadata(e) => fmt::Display::fmt(e, f),
            Error::Syn(e) => fmt::Display::fmt(e, f),
        }
    }
}
impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}
impl From<cargo_metadata::Error> for Error {
    fn from(e: cargo_metadata::Error) -> Self {
        Error::CargoMetadata(e)
    }
}
impl From<syn::Error> for Error {
    fn from(e: syn::Error) -> Self {
        Error::Syn(e)
    }
}
