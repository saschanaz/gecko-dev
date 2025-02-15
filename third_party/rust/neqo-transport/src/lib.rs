// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg_attr(feature = "deny-warnings", deny(warnings))]
#![warn(clippy::use_self)]

use neqo_common::qinfo;

mod cc;
mod cid;
mod connection;
mod crypto;
mod dump;
mod events;
mod flow_mgr;
mod frame;
mod pace;
mod packet;
mod path;
mod qlog;
mod recovery;
mod recv_stream;
mod send_stream;
pub mod server;
mod stats;
mod stream_id;
pub mod tparams;
mod tracking;

pub use self::cid::{ConnectionId, ConnectionIdManager};
pub use self::connection::{Connection, FixedConnectionIdManager, Output, State, ZeroRttState};
pub use self::events::{ConnectionEvent, ConnectionEvents};
pub use self::frame::CloseError;
pub use self::frame::StreamType;
pub use self::packet::QuicVersion;
pub use self::stream_id::StreamId;

const LOCAL_IDLE_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(30); // 30 second

type TransportError = u64;
const ERROR_APPLICATION_CLOSE: TransportError = 12;

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
#[allow(clippy::pub_enum_variant_names)]
pub enum Error {
    NoError,
    InternalError,
    ServerBusy,
    FlowControlError,
    StreamLimitError,
    StreamStateError,
    FinalSizeError,
    FrameEncodingError,
    TransportParameterError,
    ProtocolViolation,
    InvalidToken,
    ApplicationError,
    CryptoError(neqo_crypto::Error),
    QlogError,
    CryptoAlert(u8),

    // All internal errors from here.
    AckedUnsentPacket,
    ConnectionState,
    DecodingFrame,
    DecryptError,
    HandshakeFailed,
    IdleTimeout,
    IntegerOverflow,
    InvalidInput,
    InvalidMigration,
    InvalidPacket,
    InvalidResumptionToken,
    InvalidRetry,
    InvalidStreamId,
    // Packet protection keys aren't available yet, or they have been discarded.
    KeysNotFound,
    // An attempt to update keys can be blocked if
    // a packet sent with the current keys hasn't been acknowledged.
    KeyUpdateBlocked,
    NoMoreData,
    NotConnected,
    PacketNumberOverlap,
    PeerApplicationError(AppError),
    PeerError(TransportError),
    StatelessReset,
    TooMuchData,
    UnexpectedMessage,
    UnknownFrameType,
    VersionNegotiation,
    WrongRole,
}

impl Error {
    pub fn code(&self) -> TransportError {
        match self {
            Self::NoError
            | Self::IdleTimeout
            | Self::PeerError(_)
            | Self::PeerApplicationError(_) => 0,
            Self::ServerBusy => 2,
            Self::FlowControlError => 3,
            Self::StreamLimitError => 4,
            Self::StreamStateError => 5,
            Self::FinalSizeError => 6,
            Self::FrameEncodingError => 7,
            Self::TransportParameterError => 8,
            Self::ProtocolViolation => 10,
            Self::InvalidToken => 11,
            Self::ApplicationError => ERROR_APPLICATION_CLOSE,
            Self::CryptoAlert(a) => 0x100 + u64::from(*a),
            // All the rest are internal errors.
            _ => 1,
        }
    }
}

impl From<neqo_crypto::Error> for Error {
    fn from(err: neqo_crypto::Error) -> Self {
        qinfo!("Crypto operation failed {:?}", err);
        Self::CryptoError(err)
    }
}

impl From<::qlog::Error> for Error {
    fn from(_err: ::qlog::Error) -> Self {
        Self::QlogError
    }
}

impl From<std::num::TryFromIntError> for Error {
    fn from(_: std::num::TryFromIntError) -> Self {
        Self::IntegerOverflow
    }
}

impl ::std::error::Error for Error {
    fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
        match self {
            Self::CryptoError(e) => Some(e),
            _ => None,
        }
    }
}

impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "Transport error: {:?}", self)
    }
}

pub type AppError = u64;

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum ConnectionError {
    Transport(Error),
    Application(AppError),
}

impl ConnectionError {
    pub fn app_code(&self) -> Option<AppError> {
        match self {
            Self::Application(e) => Some(*e),
            _ => None,
        }
    }
}

impl From<CloseError> for ConnectionError {
    fn from(err: CloseError) -> Self {
        match err {
            CloseError::Transport(c) => Self::Transport(Error::PeerError(c)),
            CloseError::Application(c) => Self::Application(c),
        }
    }
}

pub type Res<T> = std::result::Result<T, Error>;
