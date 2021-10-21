pub trait IntrusiveLinkedList<Key>
    where Key: Copy + Eq,
{
    type Item: IntrusiveListItem<Key = Key>;

    fn get_item(&self, key: Key) -> &Self::Item;
    fn get_item_mut(&mut self, key: Key) -> &mut Self::Item;
    fn insert_item(&mut self, item: Self::Item) -> Key;
    fn remove_item(&mut self, idx: Key) -> Self::Item;

    fn next_key(&self, key: Key) -> Option<Key> {
        self.get_item(key).next()
    }
    fn prev_key(&self, key: Key) -> Option<Key> {
        self.get_item(key).next()
    }

    /// Position this item after the given item.
    fn attach_after(&mut self, after: Key, cur: Key) {
        debug_assert!(
            self.get_item(cur).is_freestanding(),
            "The value attached should be freestanding"
        );

        let after_item = self.get_item_mut(after);
        let next = after_item.next();
        after_item.set_next(Some(cur));

        let cur_item = self.get_item_mut(cur);
        cur_item.set_prev(Some(after));
        cur_item.set_next(next);

        if let Some(idx) = next {
            self.get_item_mut(idx).set_prev(Some(cur));
        };
    }

    /// Position this item before the given item.
    fn attach_before(&mut self, before: Key, this: Key) {
        debug_assert!(
            self.get_item(this).is_freestanding(),
            "The value attached should be freestanding"
        );

        let before_item = self.get_item_mut(before);
        let prev = before_item.prev();
        before_item.set_prev(Some(this));

        let current = self.get_item_mut(this);
        current.set_next(Some(before));
        current.set_prev(prev);

        if let Some(idx) = prev {
            let prev = self.get_item_mut(idx);
            prev.set_next(Some(this));
        };
    }

    /// Detaches this item from the list.
    fn detach(&mut self, idx: Key) {
        let inst = self.get_item_mut(idx);
        let next_idx = inst.take_next();
        let prev_idx = inst.take_prev();

        if let Some(prev_idx) = prev_idx {
            let prev = self.get_item_mut(prev_idx);
            prev.set_next(next_idx);
        }
        if let Some(next_idx) = next_idx {
            let next = self.get_item_mut(next_idx);
            next.set_prev(prev_idx)
        }
    }

    /// Split the chain into two after the given item, or return `None` if no
    /// item is after the given item.
    fn split_after(&mut self, idx: Key) -> Option<Key> {
        let head = self.get_item_mut(idx).take_next();
        if let Some(head) = head {
            self.get_item_mut(head).set_prev(None);
        }
        head
    }

    /// Split the chain into two before the given item, or return `None` if no
    /// item is before the given item.
    fn split_before(&mut self, idx: Key) -> Option<Key> {
        let tail = self.get_item_mut(idx).take_prev();
        if let Some(tail) = tail {
            self.get_item_mut(tail).set_next(None);
        }
        tail
    }

    fn connect(&mut self, tail: Key, head: Key) {
        debug_assert!(head != tail, "Cannot connect an item to itself");
        debug_assert!(
            self.get_item(head).prev().is_none(),
            "Head item should be the last one in chain"
        );
        debug_assert!(
            self.get_item(tail).next().is_none(),
            "Tail item should be the first one in chain"
        );

        let head_item = self.get_item_mut(head);
        head_item.set_prev(Some(tail));
        let tail_item = self.get_item_mut(tail);
        tail_item.set_next(Some(head));
    }

    /// Creates an iterator of items of Self in a chain, from item `from` to `to`.
    ///
    /// - If `from` is `None`, the resulting iterator will be empty.
    /// - If `to` is `None`, the resulting iterator will iterate until the end of chain.
    fn items_iter(&self, from: Option<Key>, to: Option<Key>) -> ItemsIter<Self, Key> {
        ItemsIter::new(self, from, to)
    }
}

/// Items for [`ImplicitLinkedList`]
pub trait IntrusiveListItem {
    type Key: Copy + Eq;

    fn next(&self) -> Option<Self::Key>;
    fn set_next(&mut self, key: Option<Self::Key>);
    fn take_next(&mut self) -> Option<Self::Key> {
        let next = self.next();
        self.set_next(None);
        next
    }

    fn prev(&self) -> Option<Self::Key>;
    fn set_prev(&mut self, key: Option<Self::Key>);
    fn take_prev(&mut self) -> Option<Self::Key> {
        let prev = self.prev();
        self.set_prev(None);
        prev
    }

    fn is_freestanding(&self) -> bool {
        self.next().is_none() && self.prev().is_none()
    }
}

/// An iterator for [`ImplicitLinkedList`]
pub struct ItemsIter<'a, Ctx, Key>
    where Ctx: ?Sized,
{
    ctx: &'a Ctx,
    curr: Option<Key>,
    tail: Option<Key>,
}

impl<'a, Ctx, Key> ItemsIter<'a, Ctx, Key>
    where Ctx: ?Sized,
{
    pub fn new(ctx: &'a Ctx, curr: Option<Key>, tail: Option<Key>) -> Self {
        Self { ctx, curr, tail }
    }
}

impl<'a, Ctx, Key> Iterator for ItemsIter<'a, Ctx, Key>
    where Ctx: IntrusiveLinkedList<Key> + ?Sized,
          Ctx::Item: 'a,
          Key: Copy + Eq,
{
    type Item = (Key, &'a Ctx::Item);

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr.is_none() || self.curr == self.tail {
            return None;
        }
        let curr = self.curr.unwrap();

        let item = self.ctx.get_item(curr);
        self.curr = item.next();
        Some((curr, item))
    }
}
