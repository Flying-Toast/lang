#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'a> {
    IntLit(IntLitExpr),
    /// Reference to a bound value
    BindRef(Ident<'a>),
    Add(Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct IntLitExpr {
    pub value: i64,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Ident<'a> {
    pub name: &'a str,
}
