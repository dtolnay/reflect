use ref_cast::RefCast;
use std::fmt::{self, Debug, Display};
use syn::{AttrStyle, Attribute};

#[allow(clippy::ptr_arg)]
pub fn debug(attrs: &Vec<Attribute>) -> &impl Debug {
    Wrapper::ref_cast(attrs)
}

#[derive(RefCast)]
#[repr(transparent)]
struct Wrapper<T>(T);

impl Debug for Wrapper<Vec<Attribute>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list()
            .entries(self.0.iter().map(Wrapper::ref_cast))
            .finish()
    }
}

impl Debug for Wrapper<Attribute> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("#")?;
        match self.0.style {
            AttrStyle::Outer => {}
            AttrStyle::Inner(_) => f.write_str("!")?,
        }
        f.write_str("[")?;
        for (i, segment) in self.0.path.segments.iter().enumerate() {
            if i > 0 || self.0.path.leading_colon.is_some() {
                f.write_str("::")?;
            }
            Display::fmt(&segment.ident, f)?;
        }
        for token in self.0.tokens.clone() {
            f.write_str(" ")?;
            Display::fmt(&token, f)?;
        }
        f.write_str("]")?;
        Ok(())
    }
}

#[test]
fn test_debug() {
    use syn::parse_quote;

    let attrs = vec![
        parse_quote!(#[derive(Debug)]),
        parse_quote!(#[doc = "..."]),
        parse_quote!(#[rustfmt::skip]),
    ];

    let actual = format!("{:#?}", debug(&attrs));
    let expected = "[\
        \n    #[derive (Debug)],\
        \n    #[doc = \"...\"],\
        \n    #[rustfmt::skip],\
        \n]";
    assert_eq!(actual, expected);
}
