use once_cell::unsync::Lazy;
use smallmap::Map;

use crate::prelude::StString;

pub type AttrMap8 = AttrMap<8>;

type AttrMapKey = StString;
type AttrMapVal = Option<String>;

pub trait HasAttribute {
    fn set_attribute<V: Into<Option<String>>>(&mut self, k: StString, v: V);
    fn get_attribute_value(&self, attr: &StString) -> Option<&Option<String>>;
    fn remove_attribute(&mut self, k: &StString) -> Option<AttrMapVal>;
}

#[macro_export]
macro_rules! impl_has_attribute {
    ($ty:ident, $storage:ident) => {
        impl $crate::utils::HasAttribute for $ty {
            fn set_attribute<V: Into<Option<String>>>(&mut self, k: StString, v: V) {
                self.$storage.insert(k, v.into());
            }

            fn get_attribute_value(&self, attr: &StString) -> Option<&Option<String>> {
                self.$storage.get(attr)
            }

            fn remove_attribute(&mut self, k: &StString) -> Option<Option<String>> {
                self.$storage.remove(k)
            }
        }
    };
}

#[derive(Debug, Default)]
pub struct AttrMap<const S: usize>(Lazy<Map<AttrMapKey, AttrMapVal>>);

impl<const S: usize> AttrMap<S> {
    #[inline]
    pub fn new() -> Self {
        Self(Lazy::new(|| Map::with_capacity(S)))
    }

    #[inline]
    pub fn get(&self, key: &AttrMapKey) -> Option<&AttrMapVal> {
        self.0.get(key)
    }

    #[inline]
    pub fn remove(&mut self, key: &AttrMapKey) -> Option<AttrMapVal> {
        self.0.remove(key)
    }

    #[inline]
    pub fn insert(&mut self, key: AttrMapKey, val: AttrMapVal) -> Option<AttrMapVal> {
        self.0.insert(key, val)
    }

    #[inline]
    pub fn contains_key(&self, key: &AttrMapKey) -> bool {
        self.0.contains_key(key)
    }
}

#[cfg(test)]
mod test {
    use super::AttrMap8;

    #[test]
    fn test() {
        let mut map = AttrMap8::new();
        assert!(!map.contains_key(&"attr1".into()));

        map.insert("attr1".into(), None);
        assert!(map.contains_key(&"attr1".into()));
    }
}
