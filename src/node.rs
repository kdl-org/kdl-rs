use std::{collections::HashMap, convert::TryFrom, fmt};

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

impl fmt::Display for KdlNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(f, 0)
    }
}

impl KdlNode {
    fn write(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write!(f, "{:indent$}", "", indent = indent)?;

        display_identifier(f, &self.name)?;
        for arg in &self.values {
            write!(f, " {}", arg)?;
        }
        for (prop, value) in &self.properties {
            write!(f, " ")?;
            display_identifier(f, prop)?;
            write!(f, "={}", value)?;
        }

        if self.children.is_empty() {
            return Ok(());
        }

        writeln!(f, " {{")?;
        for child in &self.children {
            child.write(f, indent + 4)?;
            writeln!(f)?;
        }
        write!(f, "{:indent$}}}", "", indent = indent)?;

        Ok(())
    }
}
impl fmt::Display for KdlValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use KdlValue::*;
        match self {
            Int(x) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", x),
            String(x) => display_string(f, x),
            Boolean(x) => write!(f, "{}", x),
            Null => write!(f, "null"),
        }
    }
}

fn display_identifier(f: &mut fmt::Formatter<'_>, s: &str) -> fmt::Result {
    if let Ok(("", identifier)) = crate::parser::bare_identifier(s) {
        write!(f, "{}", identifier)
    } else {
        display_string(f, s)
    }
}

fn display_string(f: &mut fmt::Formatter<'_>, s: &str) -> fmt::Result {
    write!(f, "\"")?;
    for c in s.chars() {
        match crate::parser::ESCAPE_CHARS.1.get(&c) {
            None => write!(f, "{}", c)?,
            Some(c) => write!(f, "\\{}", c)?,
        }
    }
    write!(f, "\"")?;
    Ok(())
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
    fn display_value() {
        assert_eq!("1", format!("{}", KdlValue::Int(1)));
        assert_eq!("1.5", format!("{}", KdlValue::Float(1.5)));
        assert_eq!("true", format!("{}", KdlValue::Boolean(true)));
        assert_eq!("false", format!("{}", KdlValue::Boolean(false)));
        assert_eq!("null", format!("{}", KdlValue::Null));
        assert_eq!(
            r#""foo""#,
            format!("{}", KdlValue::String("foo".to_owned()))
        );
        assert_eq!(
            r#""foo \"bar\" baz""#,
            format!("{}", KdlValue::String(r#"foo "bar" baz"#.to_owned()))
        );
    }

    #[test]
    fn display_node() {
        let mut value = KdlNode {
            name: "foo".into(),
            values: vec![1.into(), "two".into()],
            properties: HashMap::new(),
            children: vec![],
        };

        value.properties.insert("three".to_owned(), 3.into());

        assert_eq!(r#"foo 1 "two" three=3"#, format!("{}", value));
    }

    #[test]
    fn display_nested_node() {
        let value = KdlNode {
            name: "a1".into(),
            values: vec!["a".into(), 1.into()],
            properties: HashMap::new(),
            children: vec![
                KdlNode {
                    name: "b1".into(),
                    values: vec!["b".into(), 1.into()],
                    properties: HashMap::new(),
                    children: vec![KdlNode {
                        name: "c1".into(),
                        values: vec!["c".into(), 1.into()],
                        properties: HashMap::new(),
                        children: vec![],
                    }],
                },
                KdlNode {
                    name: "b2".into(),
                    values: vec!["b".into(), 2.into()],
                    properties: HashMap::new(),
                    children: vec![KdlNode {
                        name: "c2".into(),
                        values: vec!["c".into(), 2.into()],
                        properties: HashMap::new(),
                        children: vec![],
                    }],
                },
            ],
        };

        assert_eq!(
            r#"
a1 "a" 1 {
    b1 "b" 1 {
        c1 "c" 1
    }
    b2 "b" 2 {
        c2 "c" 2
    }
}"#,
            format!("\n{}", value)
        );
    }

    #[test]
    fn from() {
        assert_eq!(KdlValue::from(1), KdlValue::Int(1));
        assert_eq!(KdlValue::from(1.5), KdlValue::Float(1.5));
        assert_eq!(
            KdlValue::from("foo".to_owned()),
            KdlValue::String("foo".to_owned())
        );
        assert_eq!(KdlValue::from("bar"), KdlValue::String("bar".to_owned()));
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
