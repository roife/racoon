use std::fmt::Display;
use slotmap::{new_key_type, Key, KeyData, SlotMap};
use crate::compiler::ir::intrusive_linkedlist::{IntrusiveLinkedList, IntrusiveListItem};

macro_rules! setup_index {
    ($ty:ty) => {
        impl $ty {
            pub fn slot(self) -> u32 {
                self.data().as_ffi() as u32
            }

            pub fn from_bits(v: u64) -> Self {
                Self(KeyData::from_ffi(v))
            }

            pub fn into_bits(self) -> u64 {
                self.data().as_ffi()
            }
        }

        impl From<$ty> for u32 {
            fn from(val: $ty) -> Self {
                val.slot()
            }
        }

        impl Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "%{}{}", stringify!($ty), self.slot())
            }
        }
    };
}

new_key_type! {
    pub struct BBId;
    pub struct InstId;
}

setup_index!(BBId);
setup_index!(InstId);

impl<T, Key> IntrusiveLinkedList<Key> for SlotMap<Key, T>
    where T: IntrusiveListItem<Key = Key>,
          Key: Copy + Eq + slotmap::Key,
{
    type Item = T;

    fn get_item(&self, key: Key) -> &Self::Item {
        &self[key]
    }

    fn get_item_mut(&mut self, key: Key) -> &mut Self::Item {
        &mut self[key]
    }

    fn insert_item(&mut self, item: Self::Item) -> Key {
        self.insert(item)
    }

    fn remove_item(&mut self, idx: Key) -> Self::Item {
        self.remove(idx).unwrap()
    }
}