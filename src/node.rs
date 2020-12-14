use std::collections::HashMap;

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
}
