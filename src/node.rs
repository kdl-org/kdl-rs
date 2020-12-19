use std::{collections::HashMap, convert::TryFrom};

use crate::TryFromKdlNodeValueError;

#[derive(Debug, Clone, PartialEq)]
pub struct KdlNode {
    pub name: String,
    pub values: Vec<KdlValue>,
    pub properties: HashMap<String, KdlValue>,
    pub children: Vec<KdlNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum KdlValue {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
}

// Support conversions from base types into KdlNodeValue

impl From<i64> for KdlValue {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl From<f64> for KdlValue {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl From<String> for KdlValue {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

impl From<&str> for KdlValue {
    fn from(v: &str) -> Self {
        Self::String(v.to_owned())
    }
}

impl From<bool> for KdlValue {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl<T> From<Option<T>> for KdlValue
where
    T: Into<KdlValue>,
{
    fn from(v: Option<T>) -> Self {
        v.map_or(KdlValue::Null, |v| v.into())
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
                    $( KdlValue::$bad(_) => Err(TryFromKdlNodeValueError {
                        expected: stringify!($typ),
                        variant: stringify!($bad)
                    }), )+
                    KdlValue::Null => Err(TryFromKdlNodeValueError {
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
                    $( KdlValue::$bad(_) => Err(TryFromKdlNodeValueError {
                        expected: concat!("Option::<", stringify!($typ), ">"),
                        variant: stringify!($bad)
                    }), )+
                    KdlValue::Null => Ok(None),
                }
            }
        }
    };
    (& $($lt:lifetime)?, $typ:ty, $($tt:tt)*) => {
        impl_try_from!(<$($lt)?> & $($lt)? KdlValue => $typ, $($tt)*);
    };
    ($typ:ty, $($tt:tt)*) => {
        impl_try_from!(<> KdlValue => $typ, $($tt)*);
    };
}

impl_try_from!(i64, KdlValue::Int(v) => v; Float, String, Boolean);
impl_try_from!(&, i64, KdlValue::Int(v) => *v; Float, String, Boolean);
impl_try_from!(f64, KdlValue::Float(v) => v; Int, String, Boolean);
impl_try_from!(&, f64, KdlValue::Float(v) => *v; Int, String, Boolean);
impl_try_from!(String, KdlValue::String(v) => v; Int, Float, Boolean);
impl_try_from!(&'a, &'a str, KdlValue::String(v) => &v[..]; Int, Float, Boolean);
impl_try_from!(bool, KdlValue::Boolean(v) => v; Int, Float, String);
impl_try_from!(&, bool, KdlValue::Boolean(v) => *v; Int, Float, String);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from() {
        assert_eq!(KdlValue::from(1), KdlValue::Int(1));
        assert_eq!(KdlValue::from(1.5), KdlValue::Float(1.5));
        assert_eq!(
            KdlValue::from("foo".to_owned()),
            KdlValue::String("foo".to_owned())
        );
        assert_eq!(
            KdlValue::from("bar"),
            KdlValue::String("bar".to_owned())
        );
        assert_eq!(KdlValue::from(true), KdlValue::Boolean(true));

        assert_eq!(KdlValue::from(None::<i64>), KdlValue::Null);
        assert_eq!(KdlValue::from(Some(1)), KdlValue::Int(1));
    }

    #[test]
    fn try_from_success() {
        assert_eq!(i64::try_from(KdlValue::Int(1)), Ok(1));
        assert_eq!(i64::try_from(&KdlValue::Int(1)), Ok(1));
        assert_eq!(f64::try_from(KdlValue::Float(1.5)), Ok(1.5));
        assert_eq!(f64::try_from(&KdlValue::Float(1.5)), Ok(1.5));
        assert_eq!(
            String::try_from(KdlValue::String("foo".to_owned())),
            Ok("foo".to_owned())
        );
        assert_eq!(
            <&str as TryFrom<_>>::try_from(&KdlValue::String("foo".to_owned())),
            Ok("foo")
        );
        assert_eq!(bool::try_from(KdlValue::Boolean(true)), Ok(true));
        assert_eq!(bool::try_from(&KdlValue::Boolean(true)), Ok(true));

        assert_eq!(Option::<i64>::try_from(KdlValue::Int(1)), Ok(Some(1)));
        assert_eq!(Option::<i64>::try_from(KdlValue::Null), Ok(None));
    }

    #[test]
    fn try_from_failure() {
        // We don't expose the internal format of the error type, so let's just test the message
        // for a couple of cases.
        assert_eq!(
            format!("{}", i64::try_from(KdlValue::Float(1.5)).unwrap_err()),
            "Failed to convert from KdlNodeValue::Float to i64."
        );
        assert_eq!(
            format!(
                "{}",
                Option::<i64>::try_from(KdlValue::Float(1.5)).unwrap_err()
            ),
            "Failed to convert from KdlNodeValue::Float to Option::<i64>."
        );
    }
}
