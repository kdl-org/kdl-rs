use std::fmt::Display;

/// A specific [KDL Value](https://github.com/kdl-org/kdl/blob/main/SPEC.md#value).
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum KdlValue {
    /// A [KDL Raw String](https://github.com/kdl-org/kdl/blob/main/SPEC.md#raw-string).
    RawString(String),

    /// A [KDL String](https://github.com/kdl-org/kdl/blob/main/SPEC.md#string).
    String(String),

    /// A [KDL
    /// Number](https://github.com/kdl-org/kdl/blob/main/SPEC.md#number) in
    /// binary form (e.g. `0b010101`).
    Base2(i64),

    /// A [KDL
    /// Number](https://github.com/kdl-org/kdl/blob/main/SPEC.md#number) in
    /// octal form (e.g. `0o12345670`).
    Base8(i64),

    /// A [KDL
    /// Number](https://github.com/kdl-org/kdl/blob/main/SPEC.md#number) in
    /// decimal form (e.g. `1234567890`).
    Base10(i64),

    /// A [KDL
    /// Number](https://github.com/kdl-org/kdl/blob/main/SPEC.md#number) in
    /// decimal form (e.g. `1234567890.123`), interpreted as a Rust f64.
    Base10Float(f64),

    /// A [KDL
    /// Number](https://github.com/kdl-org/kdl/blob/main/SPEC.md#number) in
    /// hexadecimal form (e.g. `1234567890abcdef`).
    Base16(i64),

    /// A [KDL Boolean](https://github.com/kdl-org/kdl/blob/main/SPEC.md#boolean).
    Bool(bool),

    /// The [KDL Null Value](https://github.com/kdl-org/kdl/blob/main/SPEC.md#null).
    Null,
}

impl Eq for KdlValue {}

// NOTE: I know, I know. This is terrible and I shouldn't do it, but it's
// better than not being able to hash KdlValue at all.
#[allow(clippy::derive_hash_xor_eq)]
impl std::hash::Hash for KdlValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            KdlValue::RawString(val) => val.hash(state),
            KdlValue::String(val) => val.hash(state),
            KdlValue::Base2(val) => val.hash(state),
            KdlValue::Base8(val) => val.hash(state),
            KdlValue::Base10(val) => val.hash(state),
            KdlValue::Base10Float(val) => {
                let val = if val == &f64::INFINITY {
                    f64::MAX
                } else if val == &f64::NEG_INFINITY {
                    -f64::MAX
                } else if val.is_nan() {
                    // We collapse NaN to 0.0 because we're evil like that.
                    0.0
                } else {
                    *val
                };
                // Good enough to be close-ish for our purposes.
                (val.trunc() as i64).hash(state);
                (val.fract() as i64).hash(state);
            }
            KdlValue::Base16(val) => val.hash(state),
            KdlValue::Bool(val) => val.hash(state),
            KdlValue::Null => core::mem::discriminant(self).hash(state),
        }
    }
}

impl KdlValue {
    /// Returns `true` if the value is a [`KdlValue::RawString`].
    pub fn is_raw_string(&self) -> bool {
        matches!(self, Self::RawString(..))
    }

    /// Returns `true` if the value is a [`KdlValue::String`].
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
    }

    /// Returns `true` if the value is a [`KdlValue::String`] or [`KdlValue::RawString`].
    pub fn is_string_value(&self) -> bool {
        matches!(self, Self::String(..) | Self::RawString(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Base2`].
    pub fn is_base2(&self) -> bool {
        matches!(self, Self::Base2(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Base8`].
    pub fn is_base8(&self) -> bool {
        matches!(self, Self::Base8(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Base10`].
    pub fn is_base10(&self) -> bool {
        matches!(self, Self::Base10(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Base16`].
    pub fn is_base16(&self) -> bool {
        matches!(self, Self::Base16(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Base2`],
    /// [`KdlValue::Base8`], [`KdlValue::Base10`], or [`KdlValue::Base16`].
    pub fn is_i64_value(&self) -> bool {
        matches!(
            self,
            Self::Base2(..) | Self::Base8(..) | Self::Base10(..) | Self::Base16(..)
        )
    }

    /// Returns `true` if the value is a [`KdlValue::Base10Float`].
    pub fn is_base10_float(&self) -> bool {
        matches!(self, Self::Base10Float(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Base10Float`].
    pub fn is_float_value(&self) -> bool {
        matches!(self, Self::Base10Float(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Bool`].
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }

    /// Returns `true` if the value is a [`KdlValue::Null`].
    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    /// Returns `Some(&str)` if the `KdlValue` is a [`KdlValue::RawString`] or a
    /// [`KdlValue::String`], otherwise returns `None`.
    pub fn as_string(&self) -> Option<&str> {
        use KdlValue::*;
        match self {
            String(s) | RawString(s) => Some(s),
            _ => None,
        }
    }

    /// Returns `Some(i64)` if the `KdlValue` is a [`KdlValue::Base2`],
    /// [`KdlValue::Base8`], [`KdlValue::Base10`], or [`KdlValue::Base16`],
    /// otherwise returns `None`.
    pub fn as_i64(&self) -> Option<i64> {
        use KdlValue::*;
        match self {
            Base2(i) | Base8(i) | Base10(i) | Base16(i) => Some(*i),
            _ => None,
        }
    }

    /// Returns `Some(f64)` if the `KdlValue` is a [`KdlValue::Base10Float`],
    /// otherwise returns `None`.
    pub fn as_f64(&self) -> Option<f64> {
        if let Self::Base10Float(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `Some(bool)` if the `KdlValue` is a [`KdlValue::Bool`], otherwise returns `None`.
    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

impl Display for KdlValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RawString(_) => self.write_raw_string(f),
            Self::String(_) => self.write_string(f),
            Self::Base2(value) => write!(f, "0b{:b}", value),
            Self::Base8(value) => write!(f, "0o{:o}", value),
            Self::Base10(value) => write!(f, "{:?}", value),
            Self::Base10Float(value) => write!(
                f,
                "{:?}",
                if value == &f64::INFINITY {
                    f64::MAX
                } else if value == &f64::NEG_INFINITY {
                    -f64::MAX
                } else if value.is_nan() {
                    0.0
                } else {
                    *value
                }
            ),
            Self::Base16(value) => write!(f, "0x{:x}", value),
            Self::Bool(value) => write!(f, "{}", value),
            Self::Null => write!(f, "null"),
        }
    }
}

impl KdlValue {
    fn write_string(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.as_string().unwrap();
        write!(f, "\"")?;
        for char in string.chars() {
            match char {
                '\\' | '"' => write!(f, "\\{}", char)?,
                '\n' => write!(f, "\\n")?,
                '\r' => write!(f, "\\r")?,
                '\t' => write!(f, "\\t")?,
                '\u{08}' => write!(f, "\\b")?,
                '\u{0C}' => write!(f, "\\f")?,
                _ => write!(f, "{}", char)?,
            }
        }
        write!(f, "\"")?;
        Ok(())
    }
    fn write_raw_string(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let raw = self.as_string().unwrap();
        let mut consecutive = 0usize;
        let mut maxhash = 0usize;
        for char in raw.chars() {
            if char == '#' {
                consecutive += 1;
            } else if char == '"' {
                maxhash = maxhash.max(consecutive + 1);
            } else {
                consecutive = 0;
            }
        }
        write!(f, "r")?;
        write!(f, "{}", "#".repeat(maxhash))?;
        write!(f, "\"{}\"", raw)?;
        write!(f, "{}", "#".repeat(maxhash))?;
        Ok(())
    }
}

impl From<i64> for KdlValue {
    fn from(value: i64) -> Self {
        KdlValue::Base10(value)
    }
}

impl From<f64> for KdlValue {
    fn from(value: f64) -> Self {
        KdlValue::Base10Float(value)
    }
}

impl From<&str> for KdlValue {
    fn from(value: &str) -> Self {
        KdlValue::String(value.to_string())
    }
}

impl From<String> for KdlValue {
    fn from(value: String) -> Self {
        KdlValue::String(value)
    }
}

impl From<bool> for KdlValue {
    fn from(value: bool) -> Self {
        KdlValue::Bool(value)
    }
}

impl<T> From<Option<T>> for KdlValue
where
    T: Into<KdlValue>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => KdlValue::Null,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn formatting() {
        let raw = KdlValue::RawString(r###"r##"foor#"bar"#baz"##"###.into());
        assert_eq!(
            format!("{}", raw),
            r####"r###"r##"foor#"bar"#baz"##"###"####
        );

        let string = KdlValue::String("foo\n".into());
        assert_eq!(format!("{}", string), r#""foo\n""#);

        let base2 = KdlValue::Base2(0b1010_1010);
        assert_eq!(format!("{}", base2), "0b10101010");

        let base8 = KdlValue::Base8(0o12345670);
        assert_eq!(format!("{}", base8), "0o12345670");

        let base10 = KdlValue::Base10(1234567890);
        assert_eq!(format!("{}", base10), "1234567890");

        let base10float = KdlValue::Base10Float(1234567890.12345);
        assert_eq!(format!("{}", base10float), "1234567890.12345");

        let base16 = KdlValue::Base16(0x1234567890ABCDEF);
        assert_eq!(format!("{}", base16), "0x1234567890abcdef");

        let boolean = KdlValue::Bool(true);
        assert_eq!(format!("{}", boolean), "true");

        let null = KdlValue::Null;
        assert_eq!(format!("{}", null), "null");
    }
}
