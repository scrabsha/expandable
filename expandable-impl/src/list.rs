pub enum LameLinkedList<'a, T> {
    Nil,
    Cons(T, &'a LameLinkedList<'a, T>),
}

impl<'a, T> LameLinkedList<'a, T> {
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
