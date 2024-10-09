// Architectural invariant: this module contains a dead-simple linked list that
// is useful for recursive functions.

pub enum LameLinkedList<'a, T> {
    Nil,
    Cons(T, &'a LameLinkedList<'a, T>),
}

impl<T> LameLinkedList<'_, T> {
    pub(crate) fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        match self {
            LameLinkedList::Nil => Vec::new(),
            LameLinkedList::Cons(head, tail) => {
                let mut out = tail.to_vec();
                out.push(head.clone());
                out
            }
        }
    }
}
