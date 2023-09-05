/// Generator trait for how symbols are illustrated in a hover card.
pub trait HoverFormatter {
    fn to_formatted(&self) -> String;
}
