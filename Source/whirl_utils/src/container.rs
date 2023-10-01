/// A container for an item.
pub trait Container<Item> {
    fn unwrap(&self) -> Item;
}
