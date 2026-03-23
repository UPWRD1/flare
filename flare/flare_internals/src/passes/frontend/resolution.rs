pub trait MyTrait {
    type Type;
}

pub struct Implementor;

impl MyTrait for Implementor {
    type Type = ();
}

pub struct Container<T: MyTrait> {
    pub f1: T::Type,
    pub f2: T::Type,
}
pub struct Inspector {}
impl Inspector {
    fn in_container<T>(&self, mut f: impl FnMut(&Self) -> T) -> T {
        f(self)
    }

    pub fn inspect_container(
        &self,
        the_container: Container<Implementor>,
    ) -> Container<Implementor> {
        self.in_container(|_| Container {
            f2: (),
            ..the_container
        })
    }
}
