///! Utilities not directly related to parsing, lexing, or
///! language-manipulation.

/// Panic with a given message unless an expression evaluates to true.
///
/// ## Examples
/// ```should_fail
/// panic_unless!(1 + 1 == 2, "Math is broken.");
/// ```
#[macro_export]
macro_rules! panic_unless {
    ($condition:expr, $($rest:expr),+) => ({ if ! $condition { panic!($($rest),+); } });
}


#[macro_use]
pub mod intrusive;




#[doc(hidden)]
macro_rules! impl_methods {
    ($body_macro:ident; $(,)*) => {};
    /* args: (&mut self) */
    ($body_macro:ident; $method:ident(&mut self) -> $ret:ty, $($rest: tt)*) 
    => (fn $method(&mut self) -> $ret { $body_macro!($method; mut, self, ) }
        impl_methods!($body_macro; $($rest)*););

    /* args: (&mut self, ...) */
    ($body_macro:ident; $method:ident(&mut self, $($arg:ident: $typ:ty),+) -> $ret:ty, $($rest: tt)*)
        => (fn $method(&mut self, $($arg: $typ),+) -> $ret { $body_macro!($method; mut, self, $($arg),+) }
            impl_methods!($body_macro; $($rest)*););

    /* args: (&self) */
    ($body_macro:ident; $method:ident(&self) -> $ret:ty, $($rest: tt)*)
        => (fn $method(&self) -> $ret { $body_macro!($method; , self, ) }
            impl_methods!($body_macro; $($rest)*););

    /* args: (&self, ...) */
    ($body_macro:ident; $method:ident(&self, $($arg:ident: $typ:ty),+) -> $ret:ty, $($rest: tt)*)
        => (fn $method(&mut self, $($arg: $typ),+) -> $ret { $body_macro!($method; , self, $($arg),+) }
            impl_methods!($body_macro; $($rest)*););
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_for {
    ($tr:path, $what:path, $body_macro:ident, $($rest: tt)*) => (impl $tr for $what { impl_methods!($body_macro; $($rest)*); });
}
