use crate::*;

#[derive(Debug, Eq, PartialEq)]
pub struct LetBind<'a> {
    pub ident: Ident<'a>,
    pub value: Expr<'a>,
}
