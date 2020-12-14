use std::{collections::HashMap, convert::TryFrom};

use crate::TryFromKdlNodeValueError;

#[derive(Debug, Clone, PartialEq)]
pub struct KdlNode {
    pub name: String,
    pub values: Vec<KdlNodeValue>,
    pub properties: HashMap<String, KdlNodeValue>,
    pub children: Vec<KdlNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum KdlNodeValue {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
}

// Support conversions from base types into KdlNodeValue

impl From<i64> for KdlNodeValue {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl From<f64> for KdlNodeValue {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl From<String> for KdlNodeValue {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

impl From<&str> for KdlNodeValue {
    fn from(v: &str) -> Self {
        Self::String(v.to_owned())
    }
}

impl From<bool> for KdlNodeValue {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl<T> From<Option<T>> for KdlNodeValue
where
    T: Into<KdlNodeValue>,
{
    fn from(v: Option<T>) -> Self {
        v.map_or(KdlNodeValue::Null, |v| v.into())
    }
}

// Support reverse conversions using TryFrom

// Synthesizes a TryFrom impl for both the base type and an Option variant.
//
// We need the Option variant because we can't write a blanket impl due to the existing
//   impl<T, U> TryFrom<U> for T where U: Into<T>
// even though KdlNodeValue does not implement Into<Option<_>>.
macro_rules! impl_try_from {
    (<$($lt:lifetime)?> $source:ty => $typ:ty, $($good:pat => $value:expr),+; $($bad:ident),+) => {
        impl<$($lt)?> TryFrom<$source> for $typ {
            type Error = TryFromKdlNodeValueError;
            fn try_from(value: $source) -> Result<Self, Self::Error> {
                match value {
                    $( $good => Ok($value), )+
                    $( KdlNodeValue::$bad(_) => Err(TryFromKdlNodeValueError {
                        expected: stringify!($typ),
                        variant: stringify!($bad)
                    }), )+
                    KdlNodeValue::Null => Err(TryFromKdlNodeValueError {
                        expected: stringify!($typ),
                        variant: "Null"
                    }),
                }
            }
        }
        impl<$($lt)?> TryFrom<$source> for Option<$typ> {
            type Error = TryFromKdlNodeValueError;
            fn try_from(value: $source) -> Result<Self, Self::Error> {
                match value {
                    $( $good => Ok(Some($value)), )+
                    $( KdlNodeValue::$bad(_) => Err(TryFromKdlNodeValueError {
                        expected: concat!("Option::<", stringify!($typ), ">"),
                        variant: stringify!($bad)
                    }), )+
                    KdlNodeValue::Null => Ok(None),
                }
            }
        }
    };
    (& $($lt:lifetime)?, $typ:ty, $($tt:tt)*) => {
        impl_try_from!(<$($lt)?> & $($lt)? KdlNodeValue => $typ, $($tt)*);
    };
    ($typ:ty, $($tt:tt)*) => {
        impl_try_from!(<> KdlNodeValue => $typ, $($tt)*);
    };
}

impl_try_from!(i64, KdlNodeValue::Int(v) => v; Float, String, Boolean);
impl_try_from!(&, i64, KdlNodeValue::Int(v) => *v; Float, String, Boolean);
impl_try_from!(f64, KdlNodeValue::Float(v) => v; Int, String, Boolean);
impl_try_from!(&, f64, KdlNodeValue::Float(v) => *v; Int, String, Boolean);
impl_try_from!(String, KdlNodeValue::String(v) => v; Int, Float, Boolean);
impl_try_from!(&'a, &'a str, KdlNodeValue::String(v) => &v[..]; Int, Float, Boolean);
impl_try_from!(bool, KdlNodeValue::Boolean(v) => v; Int, Float, String);
impl_try_from!(&, bool, KdlNodeValue::Boolean(v) => *v; Int, Float, String);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from() {
        assert_eq!(KdlNodeValue::from(1), KdlNodeValue::Int(1));
        assert_eq!(KdlNodeValue::from(1.5), KdlNodeValue::Float(1.5));
        assert_eq!(
            KdlNodeValue::from("foo".to_owned()),
            KdlNodeValue::String("foo".to_owned())
        );
        assert_eq!(
            KdlNodeValue::from("bar"),
            KdlNodeValue::String("bar".to_owned())
        );
        assert_eq!(KdlNodeValue::from(true), KdlNodeValue::Boolean(true));

        assert_eq!(KdlNodeValue::from(None::<i64>), KdlNodeValue::Null);
        assert_eq!(KdlNodeValue::from(Some(1)), KdlNodeValue::Int(1));
    }

    #[test]
    fn try_from_success() {
        assert_eq!(i64::try_from(KdlNodeValue::Int(1)), Ok(1));
        assert_eq!(i64::try_from(&KdlNodeValue::Int(1)), Ok(1));
        assert_eq!(f64::try_from(KdlNodeValue::Float(1.5)), Ok(1.5));
        assert_eq!(f64::try_from(&KdlNodeValue::Float(1.5)), Ok(1.5));
        assert_eq!(
            String::try_from(KdlNodeValue::String("foo".to_owned())),
            Ok("foo".to_owned())
        );
        assert_eq!(
            <&str as TryFrom<_>>::try_from(&KdlNodeValue::String("foo".to_owned())),
            Ok("foo")
        );
        assert_eq!(bool::try_from(KdlNodeValue::Boolean(true)), Ok(true));
        assert_eq!(bool::try_from(&KdlNodeValue::Boolean(true)), Ok(true));

        assert_eq!(Option::<i64>::try_from(KdlNodeValue::Int(1)), Ok(Some(1)));
        assert_eq!(Option::<i64>::try_from(KdlNodeValue::Null), Ok(None));
    }

    #[test]
    fn try_from_failure() {
        // We don't expose the internal format of the error type, so let's just test the message
        // for a couple of cases.
        assert_eq!(
            format!("{}", i64::try_from(KdlNodeValue::Float(1.5)).unwrap_err()),
            "Failed to convert from KdlNodeValue::Float to i64."
        );
        assert_eq!(
            format!(
                "{}",
                Option::<i64>::try_from(KdlNodeValue::Float(1.5)).unwrap_err()
            ),
            "Failed to convert from KdlNodeValue::Float to Option::<i64>."
        );
    }
}
